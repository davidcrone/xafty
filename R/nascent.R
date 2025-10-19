
#' Retrieve Data from a xafty network
#' @param network A xafty network
#' @param ... List.
#' @export
nascent <- function(network, ...) {
  stopifnot(inherits(network, "xafty_network"))
  globals <- dots_to_query(network = network, ... = ...)
  data <- nascent_query(globals = globals, network = network)
  data
}

sort_links <- function(codes, sm) {
  links <- sm$get_links()
  lapply(codes, \(code) links[[code]])
}

nascent_query <- function(globals, network) {
  dag <- build_dag(globals = globals, network = network)
  data <- evaluate_dag(dag = dag)
  data
}

resolve_objects <- function(network, dag_sm = NULL) {
  codes <- names(dag_sm$get_codes())
  log_object <- vapply(codes, \(code) grepl("^object", code), FUN.VALUE = logical(1))
  if(!any(log_object)) return(dag_sm)
  object_codes <- codes[log_object]
  for (object_code in object_codes) {
    link <- dag_sm$get_links()[[object_code]]
    queries <- get_queries(link, temper = FALSE)
    arg_names <- names(queries)
    object_dag_list <- list()
    for (arg_name in arg_names) {
      query <- queries[[arg_name]]
      globals <- dots_to_query(network = network, query)
      object_dag <- build_dag(globals = globals, network = network, frame = object_code)
      object_dag_list[[arg_name]] <- object_dag
    }
    dag_sm$set_object(object_code = object_code, dag = object_dag_list)
  }
  dag_sm
}

resolve_function_stack <- function(sm) {
 stack_sorted <- toposort::topological_sort(sm$get_codes(), dependency_type = "follows")
 stack_prepared <- remove_join_helpers(stack_sorted)
 stack_prepared
}

remove_join_helpers <- function(stack_sorted) {
  stack_sorted[!grepl("^join.", stack_sorted)]
}

get_join_functions <- function(from, to, network, sm, state_query = NULL) {
  fun_name <- network[[from]]$joined_projects[[to]]
  link <- network[[from]]$ruleset$link[[fun_name]]
  link <- interpolate_link_queries(link = link, state_list = state_query, network = network)
  code <- build_dependency_codes(link = link, network = network, dag_sm = dag_sm)
  sm$set_nodes(link = link, code = code)
  set_objects(links = list(link), network = network, dag_sm = sm)
  # Here columns that have the same variable names be joined into one variable will be noted in the mask state variable.
  # This enables later unscope of variables that might be scoped from a different project not expected by the link queries
  lst_masked_columns <- get_masked_column_names(link)
  sm$set_mask(lst_masked_columns)
  join_id = paste0("join.", paste0(sort(c(from, to)), collapse = "."))
  look_up_joins <- setNames(list(names(code)), nm = join_id)
  list(
    link = link,
    projects = c(from, to),
    lookup = look_up_joins
  )
}

join_code_generator <- function(link, network, dag_sm) {
  fused_projects <- get_lead_projects(link)
  join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
  join_dependencies <- build_dependency_codes(link = link, network = network, dag_sm = dag_sm)
  join_list_main <- setNames(join_dependencies, join_code)
  fun_codes <- vapply(get_ordered_join_pairs(link), \(pair) paste0("join.", pair[1], ".", pair[2]), FUN.VALUE = character(1))
  join_list_help <- sapply(fun_codes, \(code) join_code, simplify = FALSE, USE.NAMES = TRUE)
  c(join_list_main, join_list_help)
}

build_join_graph <- function(network) {
  names_network <- names(network)
  # remvoves xafty_containers from the join_graph
  projects <- names_network[vapply(names_network, \(project) is.environment(network[[project]]) &
                                                             inherits(network[[project]], "xafty_project"),
                                   FUN.VALUE = logical(1))]
  project_pairs <- sapply(projects, \(project) names(network[[project]]$joined_projects), simplify = FALSE, USE.NAMES = TRUE)
  project_pairs
}

projects_not_in_join_path <- function(dag_sm, network) {
  projects <- projects <- dag_sm$get_join_projects()
  join_path <- dag_sm$get_join_path()
  projects_joined <- unique(do.call(c, join_path))
  projects[!projects %in% projects_joined]
}

get_shortest_join_path_for <- function(projects, network, sm) {
  graph <- build_join_graph(network)
  check_graph(graph = graph, check_projects = projects)
  join_paths <- sm$get_join_path()
  for (i in seq_along(projects)) {
    vec_joins <- do.call(c, join_paths)
    start <- projects[i]
    if(start %in% vec_joins) next # if project is already present in join path, the job is already done!
    if(length(vec_joins) > 0) {
      # Greedy network resolution by looking for the shortest path to a project already in the join path
      possible_paths <- lapply(vec_joins, \(end) bfs_traversal(graph, start = start, end = end))
      join_paths[[start]] <- possible_paths[[which.min(vapply(possible_paths, \(path) length(path), FUN.VALUE = numeric(1)))]]
    } else {
      # Here we check whether any project can be linked
      end <- projects[i + 1]
      join_paths[[start]] <- bfs_traversal(graph, start = start, end = end)
    }
  }
  join_paths
}

# check_projects only contains the projects that are in needed to resolve a join path
check_graph <- function(graph, check_projects) {
  projects_in_graph <- names(graph)
  for (project in check_projects) {
    if(all(!graph[[project]] %in% projects_in_graph)) {
      stop(paste0("Project: '", project, "' is not joined to any other project in the network.",
                  " Therefore, building a join path is not possible. You need to add a join function that joins '",
                  project, "' to another project within the network."))
    }
  }
}

get_chatty_link_from_network <- function(col, project, network) {
  validate_query(col = col, project = project, network = network, env_name = "variables")
  columns_subset <- network[[project]]$variables[[col]]
  network[[project]]$ruleset[["link"]][[columns_subset]]
}

get_chatty_object_from_network <- function(name, project, network) {
  validate_query(col = name, project = project, network = network, env_name = "objects")
  columns_subset <- network[[project]]$objects[[name]]
  network[[project]]$ruleset[["object"]][[columns_subset]]
}

get_chatty_func_name_from_network <- function(col, project, network, env_name = "variables") {
  validate_query(col = col, project = project, network = network, env_name = env_name)
  columns_subset <- network[[project]][[env_name]][[col]]
  columns_subset
}

execute_stack <- function(link, mask, data_sm, default_states) {
  projects <- unique(c(link$project, get_lead_projects(link)))
  executable_args <- build_executable_args(link = link, data_sm = data_sm, mask = mask, default_states = default_states)
  # Doing this to avoid too much memory use, is this necessary?
  for (project in projects) {
    if(!is.null(data_sm$get_data_key(project))) {
      data_sm$set_data(data = NULL, key = data_sm$get_data_key(project))
    }
  }
  new_key <- paste0(projects, collapse = "_")

  data <- tryCatch(
    {
      do.call(link$fun, executable_args)
    },
    error = function(e) {
      message(link$fun_name)
      stop(paste0("Error occurred: ", e$message))
    })
  if(!length(link$added_object) == 1) {
    data <- scope(data = data, link = link, mask = mask)
  }
  projects_update_key <- do.call(c, lapply(projects, \(project) {
    key <- data_sm$get_data_key(project)
    if(is.null(key)) return(project)
    data_sm$get_projects_by_key(key)
  }))
  for(proj in projects_update_key) {
    data_sm$set_data_key(project = proj, key = new_key)
  }
  data_sm$set_data(data = data, key = new_key)
}

#' Build an 'Evaluable' Data Pipeline Object
#' @description
#' When querying an object, the xafty algorithm recursively iterates through the network and obtains all functions
#' necessary. Before evaluating all functions, the xafty algorithm creates a dag-object which contains the full
#' information about dependencies. The object can then be evaluated with function: evaluate_dag
#' @param globals description
#' @returns A list
#' @export
build_dag <- function(globals, network, frame = "main") {
  dag_sm <- build_tree(network = network)
  dag_sm <- initialize_join_path(join_path = globals$join_path, network = network, dag_sm = dag_sm, state_query = globals$states)
  dag_sm <- initialize_join_projects(query_list = globals$internal, network = network, dag_sm = dag_sm)
  dag_sm <- resolve_dependencies(query_list = globals, network = network, dag_sm = dag_sm)
  dag_sm <- resolve_objects(network = network, dag_sm = dag_sm)
  topological_sorted_codes <- resolve_function_stack(sm = dag_sm)
  list_links <- sort_links(topological_sorted_codes, sm = dag_sm)
  dag <- list(
    full_query = dag_sm$get_query(),
    order_query = globals$order,
    dag = dag_sm$get_codes(),
    execution_order = topological_sorted_codes,
    sorted_links = list_links,
    join_path = dag_sm$get_join_path(),
    masked_columns = dag_sm$get_mask(),
    network_states = dag_sm$get_network_state(),
    query_states = globals$states,
    objects = dag_sm$get_objects()
  )
  dag
}

evaluate_dag <- function(dag) {
  data_sm <- data_sm()
  data_sm <- set_states(states = dag$query_states, data_sm = data_sm)
  links <- dag$sorted_links
  execution_order <- dag$execution_order
  mask <- dag$masked_columns
  default_states <- dag$network_states
  for (i in seq_along(execution_order)) {
    code <- execution_order[i]
    link <- links[[i]]
    if(grepl("^object.", code)) {
      object_key <- paste0("object.", link$project, ".", link$fun_name)
      object_dag <- dag$objects[[object_key]]
      evaluate_objects(dag = object_dag, link = link, global_data_sm = data_sm)
    } else {
      execute_stack(link = link, mask = mask, data_sm = data_sm, default_states = default_states)
    }
  }
  data_key <- get_data_key(data_sm = data_sm, dag = dag)
  data <- return_unscoped_data(data = data_sm$get_data_by_key(data_key), query = dag$order_query, dag = dag)
  data
}

evaluate_objects <- function(dag, link, global_data_sm) {
  fun <- link$fun
  args <- list()
  arg_names <- names(dag)
  for (arg in arg_names) {
    object_dag <- dag[[arg]]
    args[[arg]] <- evaluate_dag(dag = object_dag)
  }
  object_data <- do.call(fun, args)
  object_data_key <- paste0(link$project, ".", get_squared_variable(link$added_object))
  global_data_sm$set_object(object_key = object_data_key, data = object_data)
  global_data_sm
}

get_data_key <- function(data_sm, dag, network) {
  unique(vapply(get_projects(dag$full_query), \(project) data_sm$get_data_key(project), FUN.VALUE = character(1)))
}

set_states <- function(states, data_sm) {
  if(is.null(states)) return(data_sm)
  data_sm$set_states(states)
  data_sm
}

initialize_join_path <- function(join_path, network, dag_sm, state_query) {
  if(is.null(join_path)) return(invisible(dag_sm))
  browser() # Needs to be changed since get_join_functions has changed
  dag_sm$set_join_path(join_path)
  lapply(join_path, \(path) {
    from <- path[-length(path)]
    to <- path[-1]
    mapply(get_join_functions, from, to, MoreArgs = list(network = network, sm = dag_sm, state_query = state_query))
  })
  invisible(dag_sm)
}

initialize_join_projects <- function(query_list, network, dag_sm) {
  projects <- get_projects(query_list)
  if(length(projects) <= 1) return(dag_sm)
  logical_project_join <- sapply(projects, \(project) project_needs_join(project = project, query_list = query_list, network = network))
  projects_join <- projects[logical_project_join]
  dag_sm$set_join_projects(projects = projects_join)
  dag_sm
}
