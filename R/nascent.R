
#' Retrieve Data from a xafty network
#' @param network A xafty network
#' @param ... List.
#' @export
nascent <- function(network, ...) {
  stopifnot(inherits(network, "xafty_network"))
  query_list <- dots_to_query(network = network, ... = ...)

  if(inherits(query_list$internal, "xafty_object_query")) {
    data <- nascent_object(query_list = query_list, network = network)
  } else {
    data <- nascent_query(query_list = query_list, network = network, return = "df")
  }
  data
}

sort_links <- function(codes, sm) {
  links <- sm$get_links()
  lapply(codes, \(code) links[[code]])
}

nascent_query <- function(query_list, network, return = c("df", "dag")) {
  dag_sm <- build_tree(network = network)
  dag_sm <- resolve_dependencies(query_list = query_list$internal, network = network, dag_sm = dag_sm)
  dag <- build_dag(dag_sm = dag_sm)
  if(all(return == "dag")) return(dag)
  data_sm <- data_sm()
  data_sm <- set_states(states = query_list$states, data_sm = data_sm)
  data_sm <- evaluate_objects(data_sm = data_sm, dag_sm = dag_sm, network = network)
  data_sm <- evaluate_dag(dag = dag, data_sm = data_sm)
  data_key <- get_data_key(data_sm = data_sm, dag_sm = dag_sm)
  data <- return_unscoped_data(data = data_sm$get_data_by_key(data_key), query = query_list$order, sm = dag_sm)
  if(all(c("df", "dag") %in% return)) {
    return(
      list(
        dag = dag,
        data = data
      )
    )
  }
  data
}

nascent_object <- function(query_list, network) {
  query_list <- query_list$internal
  link <- get_dependend_links(query_list, network)
  fun <- link[[1]]$fun
  args <- eval_args(link[[1]], network = network)
  do.call(fun, args)
}

resolve_dependencies <- function(query_list, network, dag_sm = NULL) {
  dependencies(query_list = query_list, network = network, dag_sm = dag_sm)
  set_join_dependencies(network = network, dag_sm = dag_sm)
  build_join_bridges(dag_sm = dag_sm, network = network)
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

get_join_functions <- function(from, to, network, sm) {
  fun_name <- network[[from]]$joined_projects[[to]]
  link <- network[[from]]$ruleset$modules$link[[fun_name]]
  list_join_codes <- join_code_generator(link = link, network = network, sm = sm)

  # Three codes are set. One for the join function and two join.codes which will be used to connect the functions
  # that depend on the join
  for(i in seq_along(list_join_codes)) {
    li_codes <- list_join_codes[i]
    sm$set_nodes(link = link, code = li_codes)
  }

  # Here columns that have the same but from different projects will be noted in the mask state variable. This allows
  # later to retrieve a column that might be linked from a different project but scoped differently in the execution
  lst_masked_columns <- get_masked_column_names(link)
  sm$set_mask(lst_masked_columns)

  # Here the potential new dependencies will be resolved calling dependencies and join functions again.
  queries <- get_queries(link)
  for (query in queries) {
    dependencies(query_list = query, network = network, dag_sm = sm)
  }
}

join_code_generator <- function(link, network, sm) {
  fused_projects <- get_lead_projects(link)
  join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
  split_queries <- split_args(link = link, network = network)
  set_objects(split_queries = split_queries, dag_sm = sm)
  join_dependencies <- build_dependency_codes(link = link, split_queries = split_queries, network = network, dag_sm = sm)
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

set_join_dependencies <- function(network, dag_sm) {
  new_projects <- projects_not_in_join_path(sm = dag_sm, network = network)
  overall_projects <- get_projects(dag_sm$get_query())
  overall_projects <- overall_projects[vapply(overall_projects, \(project) inherits(network[[project]], "xafty_project"), FUN.VALUE = logical(1))]
  if(length(new_projects) > 0 & length(overall_projects) > 1) {
    join_path <- get_shortest_join_path_for(new_projects, network, sm = dag_sm)
    dag_sm$set_join_path(join_path)
    lapply(join_path, \(path) {
      from <- path[-length(path)]
      to <- path[-1]
      mapply(get_join_functions, from, to, MoreArgs = list(network = network, sm = dag_sm))
    })
  }
  invisible(dag_sm)
}

projects_not_in_join_path <- function(sm, network) {
  container_envs <- get_projects(sm$get_query())
  projects <- container_envs[vapply(container_envs, \(env_name) "xafty_project" %in% class(network[[env_name]]),
                                    FUN.VALUE = logical(1))]
  join_path <- sm$get_join_path()
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
  network[[project]]$ruleset[["modules"]][["link"]][[columns_subset]]
}

get_chatty_object_from_network <- function(name, project, network) {
  validate_query(col = name, project = project, network = network, env_name = "objects")
  columns_subset <- network[[project]]$objects[[name]]
  network[[project]]$ruleset[["modules"]][["object"]][[columns_subset]]
}

get_chatty_func_name_from_network <- function(col, project, network, env_name = "variables") {
  validate_query(col = col, project = project, network = network, env_name = env_name)
  columns_subset <- network[[project]][[env_name]][[col]]
  columns_subset
}

build_join_bridges <- function(dag_sm, network) {
  projects <- get_projects(dag_sm$get_query())
  codes <- dag_sm$get_codes()
  join_codes <- names(codes)[grepl("^join.", names(codes))]
  join_paths <- dag_sm$get_join_path()
  unresolved_join_codes <- do.call(c, lapply(codes, \(stack) stack[grepl("^join.", stack)]))
  unresolved_bridges <- unresolved_join_codes[!unresolved_join_codes %in% join_codes]

  graph_nodes <- unique(do.call(c, join_paths))
  join_graph <- sapply(graph_nodes, \(node) {
    joined_projects <- names(network[[node]]$joined_projects)
    joined_projects[joined_projects %in% graph_nodes]
    }, simplify = FALSE, USE.NAMES = TRUE)

  pairs <- lapply(seq_along(unresolved_bridges), \(i) {
    start_bridge <- unresolved_bridges[i]
    dag_sm$get_join_pairs()[[start_bridge]]
  })

  additional_paths <- lapply(pairs, \(par) bfs_traversal(join_graph, start = par[1], end = par[2]))

  resolved_paths <- sapply(additional_paths, \(path) {
    from <- path[-length(path)]
    to <- path[-1]
    do.call(c, mapply(\(from, to) {
      fun_name <- network[[from]]$joined_projects[[to]]
      link <- network[[from]]$ruleset$modules$link[[fun_name]]
      fused_projects <- get_lead_projects(link)
      join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
      join_code
      }, from, to, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }, simplify = FALSE, USE.NAMES = FALSE)
  additional_join_list <- setNames(resolved_paths, unresolved_bridges)

  for (i in seq_along(additional_join_list)) {
    add_join <- additional_join_list[i]
    dag_sm$set_nodes(link = NULL, code = add_join)
  }
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
  data <- do.call(link$fun, executable_args)
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

build_dag <- function(dag_sm) {
  topological_sorted_codes <- resolve_function_stack(sm = dag_sm)
  list_links <- sort_links(topological_sorted_codes, sm = dag_sm)
  list(
    full_query = dag_sm$get_query(),
    dag = dag_sm$get_codes(),
    execution_order = topological_sorted_codes,
    sorted_links = list_links,
    masked_columns = dag_sm$get_mask(),
    network_states = dag_sm$get_network_state()
  )
}

evaluate_dag <- function(dag, data_sm) {
  links <- dag$sorted_links
  mask <- dag$masked_columns
  default_states <- dag$network_states
  lapply(links, execute_stack, mask = mask, data_sm = data_sm, default_states = default_states)
  data_sm
}

evaluate_objects <- function(data_sm, dag_sm, network) {
  object_keys <- dag_sm$get_object_names()
  for (key in object_keys) {
    object_query <- dag_sm$get_object_query(key)
    data <- nascent(network, object_query)
    data_sm$set_object(object_key = key, data = data)
  }
  data_sm
}

get_data_key <- function(data_sm, dag_sm, network) {
  unique(vapply(get_projects(dag_sm$get_query()), \(project) data_sm$get_data_key(project), FUN.VALUE = character(1)))
}

set_states <- function(states, data_sm) {
  if(is.null(states)) return(data_sm)
  data_sm$set_states(states)
  data_sm
}
