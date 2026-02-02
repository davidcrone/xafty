
#' Retrieve Data from a xafty network
#' @param query List.
#' @param network A xafty network
#' @export
nascent <- function(query, network) {
  stopifnot(inherits(network, "xafty_network"))
  dag <- build_dag(query = query, network = network)
  data <- evaluate_dag(dag = dag)
  data
}

#' Build an 'Evaluable' Data Pipeline Object
#' @description
#' When querying an object, the xafty algorithm recursively iterates through the network and obtains all functions
#' necessary. Before evaluating all functions, the xafty algorithm creates a dag-object which contains the full
#' information about dependencies. The object can then be evaluated with function: evaluate_dag
#' @param query A xafty query list object
#' @param network A xafty network
#' @param frame Used for debugging
#' @returns A list
#' @export
build_dag <- function(query, network, frame = "main") {
  globals <- dots_to_query(network = network, query)
  dag <- build_query_dag(globals = globals, network = network)
  dag
}

build_query_dag <- function(globals, network) {
  dag_sm <- build_tree(network = network)
  dag_sm <- initialize_join_path(join_path = globals$join_path, dag_sm = dag_sm)
  dag_sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states,
                                 network = network, dag_sm = dag_sm)
  execution_order <- resolve_function_stack(dag_sm = dag_sm, network = network)
  dag <- list(
    links = dag_sm$get_links(),
    dag = dag_sm$get_codes()[execution_order],
    query = dag_sm$get_query(),
    execution_order = execution_order,
    start_query = globals$internal,
    order_query = globals$order,
    where_query = globals$where,
    join_path = dag_sm$get_join_path(),
    masked_columns = dag_sm$get_mask(),
    network_states = dag_sm$get_network_state(),
    query_states = globals$states
  )
  class(dag) <- c("list", "query_dag")
  dag
}

resolve_function_stack <- function(dag_sm, network) {
  dag <- dag_sm$get_codes()
  projects <- get_projects(dag_sm$get_query())
  stack_sorted <- toposort::topological_sort(dag, dependency_type = "follows")
  stack_prepared <- remove_join_helpers(stack_sorted)
  correct_wrappers <- clean_all_wrappers(projects = projects, order = stack_prepared, dag = dag, network = network)
  correct_wrappers
}

remove_join_helpers <- function(stack_sorted) {
  stack_sorted[!grepl("^join.", stack_sorted)]
}

get_join_functions <- function(from, to, network, sm, state_query = NULL) {
  fun_name <- network[[from]]$joined_projects[[to]]
  link <- network[[from]]$ruleset[[fun_name]]
  link <- interpolate_link_queries(link = link, state_list = state_query, network = network)
  code <- build_dependency_codes(link = link, network = network)
  sm$set_nodes(link = link, code = code)
  # Here columns that have the same variable names be joined into one variable will be noted in the mask state variable.
  # This enables later unscope of variables that might be scoped from a different project not expected by the link queries
  lst_masked_columns <- get_masked_column_names(link)
  sm$set_mask(lst_masked_columns)
  join_id <- paste0("join.", paste0(sort(c(from, to)), collapse = "."))
  look_up_joins <- setNames(list(names(code$node)), nm = join_id)
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
  query_list <- dag_sm$get_query()
  all_projects <- get_projects(query_list)
  projects <- all_projects[vapply(all_projects, project_needs_join, network = network, query_list = query_list, FUN.VALUE = logical(1))]
  join_path <- dag_sm$get_join_path()
  projects_joined <- unique(do.call(c, join_path))
  projects[!projects %in% projects_joined]
}

greedy_best_first_search <- function(projects, network, dag_sm) {
  graph <- build_join_graph(network)
  check_graph(graph = graph, check_projects = projects)
  join_paths <- dag_sm$get_join_path()
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
  data <- scope(data = data, link = link, mask = mask)
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

#' Evaluate a Network Pipeline
#' @description
#' Evaluate the pipeline created via [xafty::build_dag]
#' @param dag List-Object created by [xafty::build_dag]
#' @returns A data.frame
#' @export
evaluate_dag <- function(dag) {
  data_sm <- data_sm()
  data_sm <- set_states(states = dag$query_states, data_sm = data_sm)
  links <- dag$links
  execution_order <- dag$execution_order
  mask <- dag$masked_columns
  default_states <- dag$network_states
  for (i in seq_along(execution_order)) {
    code <- execution_order[i]
    link <- links[[code]]
    execute_stack(link = link, mask = mask, data_sm = data_sm, default_states = default_states)
  }
  data_key <- get_data_key(data_sm = data_sm, dag = dag)
  data <- data_sm$get_data_by_key(data_key)
  data <- apply_where_filter(data = data, dag = dag)
  data <- return_unscoped_data(data = data, query = dag$order_query, dag = dag)
  data
}

get_data_key <- function(data_sm, dag, network) {
  unique(vapply(get_projects(dag$query), \(project) data_sm$get_data_key(project), FUN.VALUE = character(1)))
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
  invisible(dag_sm)
}

resolve_on_entry <- function(project, network, dag_sm) {
  func_names <- network[[project]]$wrappers$on_entry
  if(is.null(func_names)) return(NULL)
  dag <- dag_sm$get_codes()
  links <- lapply(func_names, \(func_name) network[[project]]$ruleset[[func_name]])
  for (i in seq_along(links)) {
    link <- links[[i]]
    node <- build_on_entry_node(link = link, network = network, dag = dag)
    # Following only needs to be done when an argument of the wrapper has {.data}"
    if(any(has_.data(link))) {
      link <- build_.data_link(link = link, node = node$node, dag_sm = dag_sm)
    }
    dag_sm$set_nodes(link = link, code = node)
  }
  on_entry_query_list <- do.call(merge_queries, get_dependend_queries(links))
  dependencies(query_list = on_entry_query_list, state_list = NULL, network = network, dag_sm = dag_sm)
}

filter_targets_without_prefix <- function(project, targets, dag) {

  # recursive DFS to collect all upstream dependencies
  collect_deps <- function(node, dag, visited = character()) {
    if (node %in% visited) return(visited)

    visited <- c(visited, node)
    deps <- dag[[node]]

    if (is.null(deps) || length(deps) == 0) return(visited)

    for (d in deps) {
      visited <- unique(c(visited, collect_deps(d, dag, visited)))
    }
    visited
  }

  result <- c()

  for (t in targets) {
    upstream <- collect_deps(t, dag)

    # does this target avoid project-prefixed nodes?
    has_project <- any(startsWith(upstream, paste0(project, ".")))

    if (!has_project) {
      result <- c(result, t)
    }
  }

  return(result)
}

get_all_non_project_codes <- function(project, codes) {
  prefix <- paste0(project, ".")
  codes <- unlist(codes, use.names = FALSE)
  codes[!startsWith(codes, prefix)]
}

resolve_on_exit <- function(project, network, dag_sm) {
  func_names <- network[[project]]$wrappers$on_exit
  if(is.null(func_names)) return(dag_sm)
  links <- lapply(func_names, \(func_name) network[[project]]$ruleset[[func_name]])
  dag <- dag_sm$get_codes()
  for (i in seq_along(links)) {
    link <- links[[i]]
    node <- build_on_exit_node(link = link, network = network, dag = dag)
    if(any(has_.data(link))) {
      link <- build_.data_link(link = link, node = node$node, dag_sm = dag_sm)
    }
    dag_sm$set_nodes(link = link, code = node)
  }
  on_exit_query_list <- do.call(merge_queries, get_dependend_queries(links))
  dag_sm <- dependencies(query_list = on_exit_query_list, state_list = NULL, network = network, dag_sm = dag_sm)
  dag_sm
}
