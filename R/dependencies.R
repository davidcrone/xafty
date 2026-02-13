resolve_dependencies <- function(query_list, state_list, network, dag_sm = NULL) {
  dependencies(query_list = query_list, state_list = state_list, network = network, dag_sm = dag_sm)
  resolve_join_dependencies(network = network, dag_sm = dag_sm, state_list = state_list)
  resolve_wrappers(network = network, dag_sm = dag_sm, state_list = state_list)
  dag_sm
}

dependencies <- function(query_list, state_list = NULL, network, dag_sm = build_tree()) {
  # removes already visited nodes leading to an eventual termination of the recursive function
  query_list <- prune_query(query_list = query_list, compare = dag_sm$get_query())
  if (length(query_list) == 0) return(dag_sm)
  # The query is merged with queries dag_sm$set_query whose dependencies have already been resolved
  dag_sm$set_query(query_list)
  links <- get_dependend_links(query_list = query_list, network = network)
  links <- lapply(links, interpolate_link_queries, network = network, state_list = state_list)
  set_nodes(links = links, network = network, dag_sm = dag_sm)
  queries <- get_dependend_queries(links)
  new_query_list <- do.call(merge_queries, queries)
  dependencies(query_list = new_query_list, state_list = state_list, network = network, dag_sm = dag_sm)
}

resolve_join_dependencies <- function(network, dag_sm, state_list = NULL) {
  projects_new <- get_unjoined_projects(dag_sm = dag_sm, network = network)
  if(length(projects_new) == 0) return(dag_sm)
  graph <- build_join_graph(network = network, dag_sm = dag_sm)
  ## add project to a given join path
  new_paths <- list()
  for (i in seq_along(projects_new)) {
    project_add <- projects_new[i]
    new_paths[[project_add]] <- greedy_best_first_search(project_add, network, dag_sm = dag_sm, graph = graph)
  }
  paths <- unique(flatten_list(remove_empty_lists(new_paths)))
  links <- join_dependencies(paths = paths, network = network, dag_sm = dag_sm, state_list = state_list)
  queries <- get_dependend_queries(links)
  query_list <- do.call(merge_queries, queries)
  resolve_dependencies(query_list = query_list, network = network, dag_sm = dag_sm, state_list = state_list)
  invisible(dag_sm)
}

join_dependencies <- function(paths, network, dag_sm, state_list) {
  links <- lapply(paths, \(path) get_join_functions(from = path[1], to = path[2], network = network, sm = dag_sm, state_list = state_list))
  links
}

resolve_wrappers <- function(network, dag_sm, state_list) {
  unresolved <- get_unresolved_wrappers(network = network, dag_sm = dag_sm)
  if(length(unresolved) == 0) return(dag_sm)
  projects <- names(unresolved)
  query_list <- list()
  for(project in projects) {
    groups <- unresolved[[project]]
    for (group in groups) {
      dag_sm$set_wrapper_project(project = project, group = group)
      query_exit <- resolve_on_exit(group = group, project = project, network = network, dag_sm = dag_sm, state_list = state_list)
      query_entry <- resolve_on_entry(group = group, project = project, network = network, dag_sm = dag_sm, state_list = state_list)
      query_list[[paste0(group, ".", project)]] <- merge_queries(query_exit, query_entry)
    }
  }
  query_list <- do.call(merge_queries, query_list)
  dag_sm <- resolve_dependencies(query_list = query_list, state_list = state_list, network = network, dag_sm = dag_sm)
  dag_sm

  dag_sm
}

get_unresolved_wrappers <- function(network, dag_sm) {
  projects <- get_projects(dag_sm$get_query())
  has_groups <- vapply(projects, \(project) length(network[[project]]$groups) > 0, FUN.VALUE = logical(1))
  projects_groups <- names(has_groups)[has_groups]
  dag <- dag_sm$get_codes()
  if(length(projects_groups) == 0) return(list())
  li_groups <- sapply(projects_groups, \(project) {
    li_groups <- network[[project]]$groups
    groups <- names(li_groups)
    has_context <- vapply(groups, \(group) {
      prefix <- paste0(group, ".", project, ".")
      (!is.null(li_groups[[group]]$contexts$on_entry) ||
       !is.null(li_groups[[group]]$contexts$on_exit)) &
       any(startsWith(names(dag), prefix))
    },   FUN.VALUE = logical(1))
    groups[has_context]
  }, simplify = FALSE, USE.NAMES = TRUE)

  li_groups_un <- sapply(projects_groups, \(project) {
    groups <- li_groups[[project]]
    res_groups <- dag_sm$get_wrapper_project(project)
    groups[!groups %in% res_groups]
  }, simplify = FALSE, USE.NAMES = TRUE)
  li_empty_removed <- remove_empty_lists(li_groups_un)
  li_empty_removed
}

build_join_bridges <- function(network, dag_sm) {
  list_join_codes <- dag_sm$get_joins()
  if(length(list_join_codes) == 0) return(dag_sm)
  join_path <- dag_sm$get_join_path()
  li_lookup <- sapply(join_path, \(path) {
    from <- path[1]
    to <- path[2]
    fun_name <- network[[from]]$joined_projects[[to]]
    link <- network[[from]]$ruleset[[fun_name]]
    paste0(link$project, ".", fun_name)
  }, simplify = FALSE, USE.NAMES = TRUE)
  list_direct_joins <- li_lookup[names(li_lookup) %in% names(list_join_codes)]
  for (i in seq_along(list_direct_joins)) {
    join_node <- list(node = list_direct_joins[i])
    dag_sm$set_nodes(link = NULL, code = join_node)
  }
  list_inderect_joins <- list_join_codes[!names(list_join_codes) %in% names(li_lookup)]
  if(length(list_inderect_joins) <= 0) return(dag_sm)
  sub_graph <- build_sub_graph(join_path = join_path)
  join_pairs <- join_pairs(list_inderect_joins)
  # Each pair is traveresd through the sub graph resulting in the join codes that are present in li_lookup
  list_join_bfs <- lapply(join_pairs, \(pairs) {
    join_ids <- list()
    for (i in seq_along(pairs)) {
      pair <- pairs[[i]]
      projects <- bfs_traversal(graph = sub_graph, start = pair[1], end = pair[2])
      join_ids[[paste0("id_",i)]] <- vapply(seq_len(length(projects) - 1), \(i) build_join_id(c(projects[i], projects[i + 1])),
                                            FUN.VALUE = character(1), USE.NAMES = FALSE)
    }
    join_ids
  })

  join_codes <- do.call(c, list(flatten_list(list_join_bfs), use.names = TRUE))
  unresolved_codes <- names(list_inderect_joins)
  for (i in seq_along(join_codes)) {
    join <- join_codes[[i]]
    join_func <- vapply(join, \(j) li_lookup[[j]], FUN.VALUE = character(1), USE.NAMES = FALSE)
    join_node <- list(node = setNames(list(join_func), unresolved_codes[i]))
    dag_sm$set_nodes(link = NULL, code = join_node)
  }

  dag_sm
}

build_join_id <- function(projects) {
  paste0("join.", paste0(sort(c(projects)), collapse = "."))
}

join_pairs <- function(inderect_joins) {
  lapply(inderect_joins, \(join_projects) {
    pairs <- lapply(seq_len(length(join_projects) - 1), \(i) {
      c(join_projects[i], join_projects[i + 1])
    })
  })
}

prune_query <- function(query_list, compare) {
  projects_in_compare <- vapply(compare, \(q) q$from, FUN.VALUE = character(1), USE.NAMES = FALSE)
  for (query in query_list) {
    project <- query$from
    if(!project %in% projects_in_compare) next
    compare_selected <- compare[[project]]$select
    new_select <- query$select
    pruned_select <- new_select[!new_select %in% compare_selected]
    if(length(pruned_select) <= 0) {
      query_list[[project]] <- NULL
    } else {
      query_list[[project]]$select <- pruned_select
    }
  }
  query_list
}

build_sub_graph <- function(join_path) {
  # Initialize an empty list to store edges
  edges <- list()

  # Loop through each join path
  for (path in join_path) {
    # Create edges between consecutive elements in each path
    if (length(path) > 1) {
      for (i in seq_len(length(path) - 1)) {
        a <- path[i]
        b <- path[i + 1]

        # Add bidirectional connections
        edges[[a]] <- unique(c(edges[[a]], b))
        edges[[b]] <- unique(c(edges[[b]], a))
      }
    }
  }

  # Ensure all nodes appear in the graph, even if isolated
  all_nodes <- unique(unlist(join_path))
  for (node in all_nodes) {
    if (is.null(edges[[node]])) {
      edges[[node]] <- character(0)
    }
  }

  edges
}

get_dependend_links <- function(query_list, network) {
  li_links <- sapply(query_list, get_links, network = network, simplify = FALSE, USE.NAMES = FALSE)
  li_links_flattened <- flatten_list(li = li_links)
  li_links_flattened
}

get_links <- function(xafty_query, network) {
  project <- xafty_query$from
  selects <- xafty_query$select
  links <- lapply(selects, \(select) get_chatty_link_from_network(name = select, project = project, network = network))
  links
}

remove_empty_lists <- function(li) {
  li[vapply(li, \(l) length(l) > 0, FUN.VALUE = logical(1))]
}

has_empty_list <- function(li) {
  any(vapply(li, \(l) length(l) == 0, FUN.VALUE = logical(1)))
}

set_nodes <- function(links, network, dag_sm) {
  li_nodes <- lapply(links, build_dependency_codes, network = network)
  mapply(dag_sm$set_nodes, links, li_nodes, SIMPLIFY = FALSE)
  invisible(TRUE)
}

get_dependend_queries <- function(links) {
  flatten_list(remove_empty_lists(lapply(links, get_queries, which = "xafty_query")))
}

get_supplied_queries <- function(links) {
  is_query_link <- vapply(links, is_query_link, FUN.VALUE = logical(1))
  links <- links[is_query_link]
  projects <- vapply(links, \(link) link$project, FUN.VALUE = character(1))
  li_variables <- lapply(links, \(link) link$variables)
  names(li_variables) <- projects
  query(li_variables)
}
