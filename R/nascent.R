
#' Retrieve Data from a xafty network
#' @param network A xafty network
#' @param ... List.
#' @export
nascent <- function(network, ...) {
  if(inherits(network, what = "bundled_xafty_network")) {
    query <- network$query
  } else if(!inherits(list(...)[[1]], what = "xafty_query_list")) {
    query <- query(...)
  } else {
    query <- list(...)[[1]]
  }
  sub_queries <- get_sub_queries(query = query, network = network)
  query <- temper_query(query = query, network = network)
  xafty_query <- merge_queries(query)
  stopifnot(is.list(xafty_query))
  stopifnot(inherits(network, "xafty_network"))
  data_sm <- govern(network)
  tree_sm <- resolve_dependencies(xafty_list = xafty_query, network = network, sm = build_tree())
  topological_sorted_codes <- resolve_function_stack(sm = tree_sm)
  list_links <- sort_links(topological_sorted_codes, sm = tree_sm)
  lapply(list_links, execute_stack, tree_sm = tree_sm, data_sm = data_sm)
  data_keys <- unique(vapply(data_sm$get_projects(), \(project) data_sm$get_data_key(project), FUN.VALUE = character(1)))
  if (length(data_keys) > 1) {
    key_salad <- vapply(data_keys, \(key) paste0(data_sm$get_projects_by_key(key), collapse = "_"), FUN.VALUE = character(1))
    data_list <- lapply(data_keys, \(key) data_sm$get_data_by_key(key))
    names(data_list) <- key_salad
    data_list
  } else {
    return_unscoped_data(data = data_sm$get_data_by_key(data_keys), query = query, sm = tree_sm)
  }
}

sort_links <- function(codes, sm) {
  links <- sm$get_links()
  lapply(codes, \(code) links[[code]])
}

resolve_dependencies <- function(xafty_list, network, sm) {
  dependencies(xafty_list, network = network, tree_sm = sm)
  set_join_dependencies(network = network, sm = sm)
  build_join_bridges(sm = sm, network = network)
  sm
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
  list_join_codes <- join_code_generator(link = link, network = network)

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
    dependencies(query_list = query, network = network, tree_sm = sm)
  }
}

append_join_list <- function(li_codes, sm) {
  code <- names(li_codes)
  current_joins <- sm$get_join_list()
  current_dep_values <- current_joins[[code]]
  new_values <- c(current_dep_values, li_codes[[code]])
  current_joins[[code]] <- new_values
  sm$set_join_list(current_joins)
  invisible(current_joins)
}

join_code_generator <- function(link, network) {
  fused_projects <- get_lead_projects(link)
  join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
  join_dependencies <- build_dependency_codes(link = link, network = network)
  join_list_main <- setNames(join_dependencies, join_code)
  fun_codes <- vapply(get_ordered_join_pairs(link), \(pair) paste0("join.", pair[1], ".", pair[2]), FUN.VALUE = character(1))
  join_list_help <- sapply(fun_codes, \(code) join_code, simplify = FALSE, USE.NAMES = TRUE)
  c(join_list_main, join_list_help)
}

build_join_graph <- function(network) {
  names_network <- names(network)
  projects <- names_network[vapply(names_network, \(project) is.environment(network[[project]]), FUN.VALUE = logical(1))]
  project_pairs <- sapply(projects, \(project) names(network[[project]]$joined_projects), simplify = FALSE, USE.NAMES = TRUE)
  # Remove projects that have not yet been joined with other projects
  project_pairs <- project_pairs[vapply(projects, \(project) !is.null(project_pairs[[project]]), FUN.VALUE = logical(1))]
  project_pairs
}

bfs_traversal <- function(graph, start, end) {

  # Breadth-First Search
  visited <- list()
  queue <- list(list(node = as.character(start), path = as.character(c(start))))

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    node <- current$node
    path <- current$path

    if (node == as.character(end)) {
      return(path)
    }

    if (is.null(visited[[node]])) {
      visited[[node]] <- TRUE
      neighbors <- graph[[node]]

      for (neighbor in neighbors) {
        if (is.null(visited[[neighbor]])) {
          queue <- append(queue, list(list(node = neighbor, path = c(path, neighbor))))
        }
      }
    }
  }
  return(NULL)
}

set_join_dependencies <- function(network, sm) {
  new_projects <- projects_not_in_join_path(sm = sm, network = network)
  overall_projects <- get_projects(sm$get_query())
  if(length(new_projects) > 0 & length(overall_projects) > 1) {
    join_path <- get_shortest_join_path_for(new_projects, network, sm = sm)
    sm$set_join_path(join_path)
    lapply(join_path, \(path) {
      from <- path[-length(path)]
      to <- path[-1]
      mapply(get_join_functions, from, to, MoreArgs = list(network = network, sm = sm))
    })
  }
  invisible(sm)
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
  join_paths <- sm$get_join_path()
  for (i in seq_along(projects)) {
    vec_joins <- do.call(c, join_paths)
    start <- projects[i]
    if(start %in% vec_joins) next # if project is already present in join path, the job is already done!
    if(length(vec_joins) > 0) {
      possible_paths <- lapply(vec_joins, \(end) bfs_traversal(graph, start = start, end = end))
      join_paths[[start]] <- possible_paths[[which.min(vapply(possible_paths, \(path) length(path), FUN.VALUE = numeric(1)))]]
    } else {
      end <- projects[i + 1]
      join_paths[[start]] <- bfs_traversal(graph, start = start, end = end)
    }
  }
  join_paths
}

get_chatty_link_from_network <- function(col, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset$variables[[col]]
  if(is.null(columns_subset)) {
    stop(paste0("Column: ", col, " is not contained in project: ", project))
  }
  network[[project]]$ruleset[["modules"]][["link"]][[columns_subset]]
}

get_chatty_func_name_from_network <- function(col, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset$variables[[col]]
  if(is.null(columns_subset)) {
    stop(paste0("Column: ", col, " is not contained in project: ", project))
  }
  columns_subset
}

build_join_bridges <- function(sm, network) {
  projects <- get_projects(sm$get_query())
  codes <- sm$get_codes()
  join_codes <- names(codes)[grepl("^join.", names(codes))]
  join_paths <- sm$get_join_path()
  unresolved_join_codes <- do.call(c, lapply(codes, \(stack) stack[grepl("^join.", stack)]))
  unresolved_bridges <- unresolved_join_codes[!unresolved_join_codes %in% join_codes]

  graph_nodes <- unique(do.call(c, join_paths))
  join_graph <- sapply(graph_nodes, \(node) {
    joined_projects <- names(network[[node]]$joined_projects)
    joined_projects[joined_projects %in% graph_nodes]
    }, simplify = FALSE, USE.NAMES = TRUE)

  pairs <- lapply(seq_along(unresolved_bridges), \(i) {
    start_bridge <- unresolved_bridges[i]
    sm$get_join_pairs()[[start_bridge]]
  })

  additional_paths <- lapply(pairs, \(par) bfs_traversal(join_graph, start = par[1], end = par[2]))

  resolved_paths <- sapply(additional_paths, \(path) {
    from <- path[-length(path)]
    to <- path[-1]
    do.call(c, mapply(\(from, to) {
      fun_name <- network[[from]]$joined_projects[[to]]
      # TODO Ruleset is hardcoded here
      link <- network[[from]]$ruleset$modules$link[[fun_name]]
      fused_projects <- get_lead_projects(link)
      join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
      join_code
      }, from, to, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }, simplify = FALSE, USE.NAMES = FALSE)
  additional_join_list <- setNames(resolved_paths, unresolved_bridges)

  for (i in seq_along(additional_join_list)) {
    add_join <- additional_join_list[i]
    sm$set_nodes(link = NULL, code = add_join)
  }
}

execute_stack <- function(link, tree_sm, data_sm) {
  projects <- unique(c(link$project, get_lead_projects(link)))
  executable_args <-  build_executable_args(link, get_data = data_sm$get_data, mask = tree_sm$get_mask())

  # Doing this to avoid too much memory use, is this necessary?
  for (project in projects) {
    if(!is.null(data_sm$get_data_key(project))) {
      data_sm$set_data(data = NULL, key = data_sm$get_data_key(project))
    }
  }
  new_key <- paste0(projects, collapse = "_")
  data <- do.call(link$fun, executable_args)
  data <- scope(data = data, link = link, mask = tree_sm$get_mask())
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
