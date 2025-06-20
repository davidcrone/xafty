
#' Build a xafty Query
#' @description
#' A xafty query will be passed to the nascent function in order to retrieve data from the network
#' @param ... Query value. See examples for creating a simple query
#' @examples
#' query(project_name1 = c("col1", "col2"), project_name2 = c("colA"))
#' @export
query <- function(...) {
  list_pulls <- list(...)
  projects <- names(list_pulls)
  if(is.null(projects)) {
    list_pulls <- unlist(list_pulls, recursive = FALSE)
    projects <- names(list_pulls)
  }
  list_xafty_links <- lapply(projects, \(project) {
    columns <- list_pulls[[project]]
    xafty_link <- list(select = columns,
                       from = project)
    class(xafty_link) <- c("list", "xafty_query")
    xafty_link
  })
  names(list_xafty_links) <- projects
  class(list_xafty_links) <- c("list", "xafty_query_list")
  list_xafty_links
}

add_query <- function(network, ...) {
  current_query <- network$query
  new_query <- query(...)
  network$query <- c(current_query, new_query)
  if(!"bundled_xafty_network" %in% class(network)) {
    class(network) <- c(class(network), "bundled_xafty_network")
  }
  invisible(network)
}

remove_query <- function(network) {
  if("query" %in% names(network)) {
    rm("query", envir = network)
  }
  current_classes <- class(network)
  class(network) <- current_classes[current_classes != "bundled_xafty_network"]
  invisible(network)
}
