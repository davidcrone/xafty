
#' Build a xafty Query
#' @description
#' A xafty query will be passed to the nascent function in order to retrieve data from the network
#' @param ... Query value. See examples for creating a simple query
#' @examples
#' query(project_name1 = c("col1", "col2"), project_name2 = c("colA"))
#' @export
query <- function(...) {
  .list_dots <- list(...)
  list_xafty_links <- lapply(seq_along(.list_dots), \(i) {
    select <- .list_dots[i]
    project <- names(select)
    if (is.character(select) & length(select) == 1 & is.null(project)) {
      select <-  "*"
      project <- select
    } else if (is.null(project)) {
      select <- unlist(select, recursive = FALSE)
      return(do.call(sub_query, args = select))
    } else {
      select <- select[[project]]
    }
    xafty_link <- list(select = select,
                       from = project)
    class(xafty_link) <- c("list", "xafty_query")
    xafty_link
    })
  list_xafty_links <- purrr::list_flatten(list_xafty_links) # Worth the dependency?
  names(list_xafty_links) <- vapply(list_xafty_links, \(query) query$from, FUN.VALUE = character(1))
  class(list_xafty_links) <- c("list", "xafty_query_list")
  list_xafty_links
}

sub_query <- function(...) {
  .list_dots <- list(...)
  list_xafty_links <- lapply(seq_along(.list_dots), \(i) {
    select <- .list_dots[i]
    project <- names(select)
    if (is.character(select) & length(select) == 1 & is.null(project)) {
      select <-  "*"
      project <- select
    } else if (is.null(project)) {
      select <- unlist(select, recursive = FALSE)
      return(do.call(query, args = select))
    } else {
      select <- select[[project]]
    }
    xafty_link <- list(select = select,
                       from = project)
    class(xafty_link) <- c("list", "xafty_query")
    xafty_link
  })
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

temper_query <- function(query, network) {
  sapply(query, \(link) {
    project <- link$from
    if (any(link$select == "*")) {
      link$select <- names(network[[project]]$variables)
    }
    link
  }, simplify = FALSE, USE.NAMES = TRUE)
}

get_sub_queries <- function(query, network) {
  projecs <- names(query)
  sub_query_list <- lapply(projecs, \(project) {
    project_env <- network[[project]]
    if ("xafty_bundle" %in% class(project_env)) {
      return(project_env$entry(val = "query"))
    }
  })
  sub_query_list[!vapply(sub_query_list, \(query) is.null(query), FUN.VALUE = logical(1))]
}


merge_queries <- function(...) {
 # TODO
}
