
#' Retrieve Data from a xafty network
#' @param network A xafty network
#' @param ... List.
#' @export
nascent <- function(network, ...) {
  stopifnot(inherits(network, "xafty_network"))
  dag <- build_dag(network = network, ... = ...)
  data <- evaluate_dag(dag = dag)
  data
}

#' Build an 'Evaluable' Data Pipeline Object
#' @description
#' When querying an object, the xafty algorithm recursively iterates through the network and obtains all functions
#' necessary. Before evaluating all functions, the xafty algorithm creates a dag-object which contains the full
#' information about dependencies. The object can then be evaluated with function: evaluate_dag
#' @param ... A xafty query list object
#' @param network A xafty network
#' @param frame Used for debugging
#' @returns A list
#' @export
build_dag <- function(..., network, frame = "main") {
  globals <- dots_to_query(network = network, ... = ...)
  if (inherits(globals$internal, "xafty_object_query")) {
    dag <- build_object_dag(globals = globals, network = network)
  } else {
    dag <- build_query_dag(globals = globals, network = network)
  }
  dag
}

build_query_dag <- function(globals, network) {
  dag_sm <- build_tree(network = network)
  dag_sm <- initialize_join_path(join_path = globals$join_path, dag_sm = dag_sm)
  dag_sm <- initialize_join_projects(query_list = globals$internal, network = network, dag_sm = dag_sm)
  dag_sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states,
                                 network = network, dag_sm = dag_sm)
  dag_sm <- resolve_wrappers(network = network, dag_sm = dag_sm)
  topological_sorted_codes <- resolve_function_stack(sm = dag_sm)
  list_links <- sort_links(topological_sorted_codes, sm = dag_sm)
  dag <- list(
    start_query = globals$internal,
    order_query = globals$order,
    full_query = dag_sm$get_query(),
    dag = dag_sm$get_codes(),
    execution_order = topological_sorted_codes,
    sorted_links = list_links,
    join_path = dag_sm$get_join_path(),
    masked_columns = dag_sm$get_mask(),
    network_states = dag_sm$get_network_state(),
    query_states = globals$states,
    objects = dag_sm$get_objects()
  )
  class(dag) <- c("list", "query_dag")
  dag
}

build_object_dag <- function(globals, network) {
  object_query <- globals$internal
  project <- object_query[[1]]$from
  object_name <- get_squared_variable(object_query[[1]]$select)
  object_link <- get_chatty_link_from_network(name = object_name, project = project, network = network)
  object_query <- get_queries(link = object_link, temper = TRUE, state_list = globals$states, network = network)
  object_args <- names(object_query)
  dag_args <- sapply(object_query, build_dag, network = network, simplify = FALSE, USE.NAMES = TRUE)
  object_dag <- list(
    name = object_name,
    project = project,
    object_link = object_link,
    args = dag_args
  )
  class(object_dag) <- c("list", "object_dag")
  object_dag
}

sort_links <- function(codes, sm) {
  links <- sm$get_links()
  lapply(codes, \(code) links[[code]])
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
      object_dag <- build_dag(query, network = network, frame = object_code)
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
  link <- network[[from]]$ruleset[[fun_name]]
  link <- interpolate_link_queries(link = link, state_list = state_query, network = network)
  code <- build_dependency_codes(link = link, network = network, dag_sm = sm)
  sm$set_nodes(link = link, code = code)
  set_objects(links = list(link), network = network, dag_sm = sm)
  # Here columns that have the same variable names be joined into one variable will be noted in the mask state variable.
  # This enables later unscope of variables that might be scoped from a different project not expected by the link queries
  lst_masked_columns <- get_masked_column_names(link)
  sm$set_mask(lst_masked_columns)
  join_id <- paste0("join.", paste0(sort(c(from, to)), collapse = "."))
  look_up_joins <- setNames(list(names(code)), nm = join_id)
  list(
    link = link,
    projects = c(from, to),
    lookup = look_up_joins
  )
}

build_join_graph <- function(network) {
  names_network <- names(network)
  projects <- names_network[vapply(names_network, \(project) is.environment(network[[project]]), FUN.VALUE = logical(1))]
  project_pairs <- sapply(projects, \(project) names(network[[project]]$joined_projects), simplify = FALSE, USE.NAMES = TRUE)
  project_pairs
}

projects_not_in_join_path <- function(dag_sm, network) {
  projects <- dag_sm$get_join_projects()
  join_path <- dag_sm$get_join_path()
  projects_joined <- unique(do.call(c, join_path))
  projects[!projects %in% projects_joined]
}

greedy_best_first_search <- function(projects, network, sm) {
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

get_chatty_link_from_network <- function(name, project, network) {
  validate_query(name = name, project = project, network = network)
  func_name <- network[[project]]$variables[[name]]
  network[[project]]$ruleset[[func_name]]
}

get_chatty_func_name_from_network <- function(name, project, network) {
  validate_query(name = name, project = project, network = network)
  columns_subset <- network[[project]]$variables[[name]]
  columns_subset
}

execute_stack <- function(link, mask, data_sm, default_states) {
  projects <- unique(c(link$project, get_lead_projects(link, which = "xafty_query")))
  executable_args <- build_executable_args(link = link, data_sm = data_sm, mask = mask, default_states = default_states)
  # Doing this to avoid too much memory use, is this necessary?
  for (project in projects) {
    if(!is.null(data_sm$get_data_key(project))) {
      data_sm$set_data(data = NULL, key = data_sm$get_data_key(project))
    }
  }
  new_key <- paste0(projects, collapse = "_")
  # message(link$fun_name)
  data <- tryCatch(
    {
      do.call(link$fun, executable_args)
    },
    error = function(e) {
      stop(paste0("Error occurred: ", e$message))
    })
  if(!is_object_link(link)) {
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

evaluate_dag <- function(dag) {
  data_sm <- data_sm()
  if(inherits(dag, "object_dag")) {
    object_fun <- dag$object_link$fun
    evaluated_args <- sapply(dag$args, evaluate_dag, simplify = FALSE, USE.NAMES = TRUE)
    data <- do.call(object_fun, evaluated_args)
    return(data)
  }
  data_sm <- set_states(states = dag$query_states, data_sm = data_sm)
  data_sm <- evaluate_objects(dag = dag$objects, global_data_sm = data_sm)
  links <- dag$sorted_links
  execution_order <- dag$execution_order
  mask <- dag$masked_columns
  default_states <- dag$network_states
  for (i in seq_along(execution_order)) {
    code <- execution_order[i]
    link <- links[[i]]
    execute_stack(link = link, mask = mask, data_sm = data_sm, default_states = default_states)
  }
  data_key <- get_data_key(data_sm = data_sm, dag = dag)
  data <- return_unscoped_data(data = data_sm$get_data_by_key(data_key), query = dag$order_query, dag = dag)
  data
}

evaluate_objects <- function(dags, global_data_sm) {
  dags_evaluated <- sapply(dags, evaluate_dag, simplify = FALSE, USE.NAMES = TRUE)
  object_keys <- names(dags_evaluated)
  for (object_key in object_keys) {
    global_data_sm$set_object(object_key = object_key, data = dags_evaluated[[object_key]])
  }
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

initialize_join_path <- function(join_path, dag_sm) {
  if(is.null(join_path)) return(invisible(dag_sm))
  dag_sm$set_join_path(join_path)
  join_projects <- unique(unlist(join_path, recursive = TRUE, use.names = FALSE))
  dag_sm$set_join_projects(projects = join_projects)
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

resolve_wrappers <- function(network, dag_sm) {
  projects <- get_projects(dag_sm$get_query())
  has_wrappers <- vapply(projects, \(project) !is.null(c(network[[project]]$wrappers$on_entry,
                                                         network[[project]]$wrappers$on_exit)), FUN.VALUE = logical(1))
  if(all(!has_wrappers)) return(dag_sm)
  projects_w_wrappers <- projects[has_wrappers]
  for (project in projects_w_wrappers) {
    resolve_on_entry(project = project, network = network, dag_sm = dag_sm)
  }
  dag_sm
}

resolve_on_entry <- function(project, network, dag_sm) {
  func_names <- network[[project]]$wrappers$on_entry
  codes <- dag_sm$get_codes()
  links <- lapply(func_names, \(func_name) network[[project]]$ruleset[[func_name]])
  on_entry_codes <- vapply(links, build_fun_code, FUN.VALUE = character(1))
  for (i in seq_along(links)) {
    link <- links[[i]]
    on_entry_code <- on_entry_codes[i]
    project_codes <- codes[grepl(paste0("^", project, "."), names(codes))]
    deps <- clean_wrapper_deps(on_entry_code = on_entry_code, on_entry_codes = on_entry_codes, project_codes = project_codes)
    deps_funcs <- names(project_codes)
    all_links <- dag_sm$get_links()
    dep_links <- lapply(deps_funcs, \(code) all_links[[code]])
    dep_queries <- get_dependend_queries(dep_links)
    link$args <- sapply(link$args, \(arg) {
      if(!is.character(arg)) return(arg)
      if(all(arg == "{.data}")) {
        do.call(merge_queries, dep_queries)
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
    fun_node <- setNames(list(deps), on_entry_code)
    dag_sm$set_nodes(link = link, code = fun_node)
  }
}

clean_wrapper_deps <- function(on_entry_code, on_entry_codes, project_codes) {
  deps_w_wrapper <- unique(unlist(project_codes, use.names = FALSE))
  from <- which(on_entry_codes == on_entry_code)
  to <- length(on_entry_codes)
  remove_codes <- on_entry_codes[seq(from = from, to)]
  deps_w_wrapper[!deps_w_wrapper %in% remove_codes]
}
