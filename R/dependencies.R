resolve_dependencies <- function(query_list, context_list, state_list, network, dag_sm = NULL) {
  query_list <- append(query_list, context_list)
  dependencies(query_list = query_list, state_list = state_list, network = network, dag_sm = dag_sm)
  resolve_join_dependencies(network = network, dag_sm = dag_sm, state_list = state_list)
  dag_sm
}

dependencies <- function(query_list, state_list = NULL, network, dag_sm = build_tree()) {
  if (length(query_list) == 0) return(dag_sm)
  # The query is merged with queries dag_sm$set_query whose dependencies have already been resolved
  dag_sm$set_query(query_list)
  links <- get_dependend_links(query_list = query_list, network = network)
  links <- lapply(links, interpolate_link_queries, network = network, state_list = state_list)
  set_nodes(links = links, network = network, dag_sm = dag_sm)
  set_objects(links = links, network = network, dag_sm = dag_sm)
  queries <- get_dependend_queries(links)
  new_query_list <- do.call(merge_queries, queries)
  # removes already visited nodes leading to an eventual termination of the recursive function
  query_pruned <- prune_query(query_list = new_query_list, compare = dag_sm$get_query())
  dependencies(query_list = query_pruned, state_list = state_list, network = network, dag_sm = dag_sm)
}

# Currently re-calling resolve_join_dependencies is inefficient, because if the join_path has not changed,
# running join_dependencies again is unnecessary which would also run dependencies again. running join dependencies again
# would only be necessary when the dependencies of the join-dependencies would need a totally different join not yet in the join_path
# resolve_join_dependencies would need to be rewritten to only
resolve_join_dependencies <- function(network, dag_sm, state_list = NULL) {
  new_projects <- projects_not_in_join_path(dag_sm = dag_sm, network = network)
  if(length(new_projects) > 1) {
    join_path <- greedy_best_first_search(new_projects, network, sm = dag_sm)
    dag_sm$set_join_path(join_path)
  } else {
    join_path <- dag_sm$get_join_path()
  }
  links <- join_dependencies(join_path = join_path, network = network, dag_sm = dag_sm, state_query = state_list)
  queries <- get_dependend_queries(links)
  query_list <- do.call(merge_queries, queries)
  dependencies(query_list = query_list, network = network, dag_sm = dag_sm)
  invisible(dag_sm)
}

join_dependencies <- function(join_path, network, dag_sm, state_query) {
  li_joins <- lapply(join_path, \(path) {
    from <- path[-length(path)]
    to <- path[-1]
    mapply(get_join_functions, from, to, MoreArgs = list(network = network, sm = dag_sm, state_query = state_query), SIMPLIFY = FALSE)
  })
  li_joins <- flatten_list(li_joins)
  li_lookup <- build_flat_lookup(li_joins = li_joins)
  build_join_bridges(li_lookup = li_lookup, network = network, dag_sm = dag_sm)
  # Here the potential new dependencies will be resolved calling dependencies and join functions again.
  links <- lapply(li_joins, \(li_join) li_join$link)
  links
}

build_flat_lookup <- function(li_joins) {
  li_lookup <- list()
  for (li_join in li_joins) {
    join_id <- names(li_join$lookup)
    li_lookup[[join_id]] <- li_join$lookup[[join_id]]
  }
  li_lookup
}

build_join_bridges <- function(li_lookup, network, dag_sm) {
  list_join_codes <- dag_sm$get_joins()
  list_direct_joins <- li_lookup[names(li_lookup) %in% names(list_join_codes)]
  for (i in seq_along(list_direct_joins)) {
    join_node <- list_direct_joins[i]
    dag_sm$set_nodes(link = NULL, code = join_node)
  }
  list_inderect_joins <- list_join_codes[!names(list_join_codes) %in% names(li_lookup)]
  if(length(list_inderect_joins) <= 0) return(dag_sm)
  list_join_path <- dag_sm$get_join_path()
  sub_graph <- build_sub_graph(join_path = list_join_path)
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
    join_node <- setNames(list(join_func), unresolved_codes[i])
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
  if(any(is_object_variable(selects))) selects <- get_squared_variable(selects)
  links <- lapply(selects, \(select) get_chatty_link_from_network(name = select, project = project, network = network))
  links
}

remove_empty_lists <- function(li) {
  li[vapply(li, \(l) length(l) > 0, FUN.VALUE = logical(1))]
}

set_nodes <- function(links, network, dag_sm) {
  codes <- lapply(links, build_dependency_codes, network = network, dag_sm = dag_sm)
  mapply(dag_sm$set_nodes, links, codes, SIMPLIFY = FALSE)
  invisible(TRUE)
}

set_objects <- function(links, network, dag_sm) {
  for (link in links) {
    xafty_objects <- get_xafty_objects_vec(link)
    logical_vec <- xafty_objects == "xafty_object"
    has_object_query <- any(logical_vec)
    if(!has_object_query) next
    object_queries <-link$args[logical_vec]
    for(object_query in object_queries) {
      object_link <- get_links(object_query[[1]], network)
      object_code <- setNames(list(character(0)), paste0("object.", object_query[[1]]$from, ".", object_link[[1]]$fun_name))
      dag_sm$set_nodes(link = object_link[[1]], code = object_code)
    }
  }
}

get_dependend_queries <- function(links) {
  flatten_list(remove_empty_lists(lapply(links, get_queries, which = "xafty_query")))
}
