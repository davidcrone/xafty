
#' Retrieve Data from a xafty network
#' @param network A xafty network
#' @param ... List.
#' @export
nascent <- function(network, ...) {
  if(inherits(network, what = "bundled_xafty_network")) {
    xafty_query <- network$query
  } else if(!inherits(list(...)[[1]], what = "xafty_query_list")) {
    xafty_query <- query(...)
  } else {
    xafty_query <- list(...)[[1]]
  }
  sub_queries <- get_sub_queries(query = xafty_query, network = network)
  xafty_query <- temper_query(query = xafty_query, network = network)

  stopifnot(is.list(xafty_query))
  stopifnot(inherits(network, "xafty_network"))
  sm <- govern(network)
  projects <- names(xafty_query)
  resolve_dependencies(projects = projects, xafty_list = xafty_query, network = network, sm = sm)
  topological_sorted_codes <- resolve_function_stack(sm = sm)
  list_function_stack <- lapply(topological_sorted_codes, retrieve_functions, sm = sm, env = network, module = "link")
  lapply(list_function_stack, sm$execute_stack)
  data_keys <- unique(vapply(sm$get_projects(), \(project) sm$get_data_key(project), FUN.VALUE = character(1)))
  if (length(data_keys) > 1) {
    key_salad <- vapply(data_keys, \(key) paste0(sm$get_projects_by_key(key), collapse = "_"), FUN.VALUE = character(1))
    data_list <- lapply(data_keys, \(key) sm$get_data_by_key(key))
    names(data_list) <- key_salad
    data_list
  } else {
    sm$get_data_by_key(data_keys)[collect_all_column_names(xafty_query)]
  }
}

retrieve_functions <- function(code, module = "link", sm, env) {
  pair <- sm$get_fun_pair(code)
  project <- pair$project
  fun <- pair$fun
  env[[project]]$ruleset$modules[[module]][[fun]]
}


collect_all_column_names <- function(xafty_list) {
  do.call(c, lapply(xafty_list, \(xl) xl$select))
}

resolve_dependencies <- function(projects, xafty_list, network, sm) {
  lapply(projects, \(project) {
    list_links <- xafty_list[[project]]
    pulls <- list_links$select
    list_link <- sapply(pulls, get_chatty_link_from_network, project = project, network = network, simplify = FALSE, USE.NAMES = TRUE)
    add_function_stack <- lapply(pulls, \(pull) {
      link <- list_link[[pull]]
      fun_code <- paste0(project, ".", link$ruleset$fun_name) # This happens here because when setting a join function, it needs to add join as a prefix.
      sm$set_function_stack(project = project, fun_name = link$ruleset$fun_name, fun_code = fun_code, deps = link$network$dependencies, push = link$network$output$added_cols)
    })
    dependend_projects <- lapply(list_link, \(link) {
       # TODO this needs a better break condition, so that normal arguments are not looped through
      dependencies <- link$network$dependencies
      arg_names <- link$network$arg_defs$names
      if(length(dependencies) > 0) {
        for (name in arg_names) {
          deps_arg <- dependencies[[name]]
          projects <- names(deps_arg$cols)
          for (proj in projects) {
            add_columns <- deps_arg$cols[[proj]]$select
            sm$set_dependencies(proj, add_columns)
          }
        }
      }
    })
  })
  set_join_dependencies(projects = sm$get_projects(), network = network, sm = sm)
  # Check whether unresolved dependencies remain
  list_dependencies <- sapply(sm$get_projects(), \(project) sm$get_dependencies(project), simplify = FALSE, USE.NAMES = TRUE)
  list_dependencies <- list_dependencies[vapply(list_dependencies, \(value) !is.null(value), FUN.VALUE = logical(1))]
  if (length(list_dependencies) > 0) {
    projects <- names(list_dependencies)
    xafty_list <- do.call(query, list_dependencies)
    resolve_dependencies(projects = projects, xafty_list = xafty_list, network = network, sm = sm)
  }
  build_join_bridges(sm = sm, network = network)
}

resolve_function_stack <- function(sm) {
 list_stack_projects <- lapply(sm$get_projects(), sm$get_function_stack)
 stack_unsorted <- do.call(c, list_stack_projects)
 stack_appended <- append(stack_unsorted, sm$get_join_list())
 stack_sorted <- toposort::topological_sort(stack_appended, dependency_type = "follows")
 stack_prepared <- remove_join_helpers(stack_sorted)
 stack_prepared
}

remove_join_helpers <- function(stack_sorted) {
  stack_sorted[!grepl("^join.", stack_sorted)]
}

get_join_functions <- function(from, to, network, sm) {
  fun_name <- network[[from]]$joined_projects[[to]]
  link <- network[[from]]$ruleset$modules$link[[fun_name]]
  arg_names <- link$network$arg_defs$names
  fused_projects <- vapply(arg_names, \(name) link$network$dependencies[[name]]$lead, FUN.VALUE = character(1))
  join_code <- paste0("fuse.", paste0(fused_projects, collapse = "."))
  sm$set_fun_pair(project = from, fun = fun_name, code = join_code)
  list_join_codes <- join_code_generator(link = link, join_code = join_code, fused_projects = fused_projects)
  for(i in seq_along(list_join_codes)) {
    li_codes <- list_join_codes[i]
    append_join_list(li_codes, sm = sm)
  }
  dependencies <- link$network$dependencies
  for (name in arg_names) {
    deps_arg <- dependencies[[name]]$cols
    projects <- names(deps_arg)
    for (proj in projects) {
      join_dependencies <- deps_arg[[proj]]$select
      sm$set_dependencies(project = proj, cols = join_dependencies)
    }
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

join_code_generator <- function(link, join_code, fused_projects) {
  join_dependencies <- build_dependency_codes(deps = link$network$dependencies)
  join_list_main <- setNames(list(join_dependencies), join_code)
  fun_codes <- vapply(get_ordered_join_pairs(fused_projects), \(pair) paste0("join.", pair[1], ".", pair[2]), FUN.VALUE = character(1))
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

set_join_dependencies <- function(projects, network, sm) {
  if(length(projects) > 1 & !all(projects %in% unique(do.call(c, sm$get_join_path())))) {
    join_path <- get_shortest_join_path_for(projects, network)
    sm$set_join_list(NULL)
    sm$set_join_path(join_path)
    lapply(join_path, \(path) {
      from <- path[-length(path)]
      to <- path[-1]
      mapply(get_join_functions, from, to, MoreArgs = list(network = network, sm = sm))
    })
  }
  invisible(NULL)
}

get_shortest_join_path_for <- function(projects, network) {
  graph <- build_join_graph(network)
  join_paths <- list()
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

get_chatty_link_from_network <- function(pull, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset$variables[[pull]]
  if(is.null(columns_subset)) {
    stop(paste0("Column: ", pull, " is not contained in project: ", project))
  }
  network[[project]]$ruleset[["modules"]][["link"]][[columns_subset]]
}

build_join_bridges <- function(sm, network) {
  projects <- sm$get_projects()
  join_names <- names(sm$get_join_list())
  join_paths <- sm$get_join_path()
  join_codes <- join_names[grepl("^join.", join_names)]
  function_stack <- do.call(c, lapply(projects, \(project) {
    stack <- sm$get_function_stack(project)
    len <- length(stack)
    names_stack <- rep(project, len)
    setNames(stack, names_stack)
  }))
  unresolved_join_codes <- do.call(c, lapply(function_stack, \(stack) stack[grepl("^join.", stack)]))
  unresolved_bridges <- unresolved_join_codes[!unresolved_join_codes %in% join_codes]

  graph_nodes <- unique(do.call(c, join_paths))
  join_graph <- sapply(graph_nodes, \(node) {
    joined_projects <- names(network[[node]]$joined_projects)
    joined_projects[joined_projects %in% graph_nodes]
    }, simplify = FALSE, USE.NAMES = TRUE)

  pairs <- lapply(seq_along(unresolved_bridges), \(i) {
    start_bridge <- unresolved_bridges[i]
    start <- names(start_bridge)
    start_code <- paste0("join.", start, ".")
    end <- sub(start_code, replacement = "", start_bridge)
    c(start, end)
  })

  additional_paths <- lapply(pairs, \(par) bfs_traversal(join_graph, start = par[1], end = par[2]))

  resolved_paths <- sapply(additional_paths, \(path) {
    from <- path[-length(path)]
    to <- path[-1]
    do.call(c, mapply(\(from, to) {
      link <- network[[from]]$joined_projects[[to]]
      paste0("fuse.", link$left, ".", link$right)
      }, from, to, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }, simplify = FALSE, USE.NAMES = FALSE)
  additional_join_list <- setNames(resolved_paths, unresolved_bridges)

  sm$set_join_list(append(sm$get_join_list(), additional_join_list))
}

