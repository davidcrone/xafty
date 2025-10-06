

dependencies <- function(query_list, state_list = NULL, network, dag_sm = build_tree()) {
  # The query is merged with queries whose dependencies have already been resolved
  dag_sm$set_query(query_list)
  links <- get_dependend_links(query_list = query_list, network = network)
  links <- lapply(links, interpolate_link_queries, network = network, state_list = state_list)
  set_nodes(links = links, network = network, dag_sm = dag_sm)
  set_objects(links = links, network = network, dag_sm = dag_sm)
  queries <- get_dependend_queries(links)
  new_query_list <- do.call(merge_queries, queries)
  if (length(new_query_list) == 0) return(dag_sm)
  # removes already visited nodes leading to an eventual termination of the recursive function
  query_pruned <- prune_query(query = new_query_list, compare = dag_sm$get_query())
  dependencies(query_list = query_pruned, state_list = state_list, network = network, dag_sm = dag_sm)
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

get_dependend_links <- function(query_list, network) {
  li_links <- sapply(query_list, get_links, network = network, simplify = FALSE, USE.NAMES = FALSE)
  li_links_flattened <- flatten_list(li = li_links)
  li_links_flattened
}

get_links <- function(xafty_query, network) {
  project <- xafty_query$from
  selects <- xafty_query$select
  links <- lapply(selects, \(select) {
              if(is_xafty_object_variable(select)) {
                object_name <- get_squared_variable(select)
                get_chatty_object_from_network(name = object_name, project = project, network = network)
              } else {
                get_chatty_link_from_network(col = select, project = project, network = network)
              }
           })
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
