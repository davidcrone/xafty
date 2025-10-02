

dependencies <- function(query_list, network, dag_sm = build_tree()) {
  # The query is merged with queries whose dependencies have already been resolved
  dag_sm$set_query(query_list)
  links <- get_dependend_links(query_list, network)
  split_queries <- lapply(links, split_args, network = network) # each link has their queries split into queries and object queries
  codes <- mapply(build_dependency_codes, links, split_queries, MoreArgs = list(network = network, dag_sm = dag_sm), SIMPLIFY = FALSE)
  set_nodes(links = links, codes = codes, dag_sm = dag_sm)
  set_objects(split_queries = split_queries, dag_sm = dag_sm)
  queries <- flatten_list(lapply(split_queries, \(query) query$xafty_query))
  query <- do.call(merge_queries, queries)
  if (length(query) == 0) return(dag_sm)
  # removes already visited nodes leading to an eventual termination of the recursive function
  query_pruned <- prune_query(query = query, compare = dag_sm$get_query())
  dependencies(query_list = query_pruned, network = network, dag_sm = dag_sm)
}

prune_query <- function(query, compare) {
  projects_in_compare <- vapply(compare, \(q) q$from, FUN.VALUE = character(1), USE.NAMES = FALSE)
  for (q in query) {
    project <- q$from
    if(!project %in% projects_in_compare) next
    compare_selected <- compare[[project]]$select
    new_select <- q$select
    pruned_select <- new_select[!new_select %in% compare_selected]
    if(length(pruned_select) <= 0) {
      query[[project]] <- NULL
    } else {
      query[[project]]$select <- pruned_select
    }
  }
  query
}

get_dependend_links <- function(query, network) {
  li_links <- sapply(query, get_links, network = network, simplify = FALSE, USE.NAMES = FALSE)
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

set_nodes <- function(links, codes, dag_sm) {
  mapply(dag_sm$set_nodes, links, codes, SIMPLIFY = FALSE)
  invisible(TRUE)
}

set_objects <- function(split_queries, dag_sm) {
  split_queries <- remove_empty_lists(split_queries)
  if(!length(split_queries) == 0) {
    lapply(split_queries, \(query) dag_sm$set_objects(query$xafty_object))
  }
}
