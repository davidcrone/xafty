

dependencies <- function(query_list, network, tree_sm = build_tree()) {
  tree_sm$set_query(query_list)
  links <- get_dependend_links(query_list, network)
  codes <- lapply(links, build_dependency_codes, network = network)
  out <- mapply(tree_sm$set_nodes, links, codes, SIMPLIFY = FALSE)
  queries <- flatten_list(remove_empty_lists(get_dependend_queries(links = links)))
  query <- do.call(merge_queries, queries)
  if (length(query) == 0) return(tree_sm)
  query_pruned <- prune_query(query = query, compare = tree_sm$get_query())
  dependencies(query_list = query_pruned, network = network, tree_sm = tree_sm)
}

prune_query <- function(query, compare) {
  projects_in_compare <- vapply(compare, \(q) q$from, FUN.VALUE = character(1), USE.NAMES = FALSE)
  for (q in query) {
    project <- q$from
    if(!project %in% projects_in_compare) next
    compare_selected <- compare[[project]]$select
    new_select <- q$select
    pruned_select <- new_select[!new_select %in% compare_selected]
    query[[project]]$select <- pruned_select
  }
  query
}

get_dependend_queries <- function(links) {
  lapply(links, get_queries)
}

get_dependend_links <- function(query, network) {
  li_links <- sapply(query, get_links, network = network, simplify = FALSE, USE.NAMES = FALSE)
  li_links_flattened <- flatten_list(li = li_links)
  li_links_flattened
}

get_links <- function(xafty_query, network) {
  project <- xafty_query$from
  selects <- xafty_query$select
  links <- lapply(selects, get_chatty_link_from_network, project = project, network = network)
  links
}

remove_empty_lists <- function(li) {
  li[vapply(li, \(l) length(l) > 0, FUN.VALUE = logical(1))]
}
