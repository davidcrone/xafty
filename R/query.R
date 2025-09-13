
#' Build a xafty Query
#' @description
#' A xafty query will be passed to the nascent function in order to retrieve data from the network
#' @param ... Query value. See examples for creating a simple query
#' @examples
#' query(project_name1 = c("col1", "col2"), project_name2 = c("colA"))
#' @export
query <- function(...) {
  .list_dots <- list(...)
  query_list <- lapply(seq_along(.list_dots), \(i) {
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
  query_list <- purrr::list_flatten(query_list) # Worth the dependency?
  names(query_list) <- vapply(query_list, \(query) query$from, FUN.VALUE = character(1))
  if(has_misuse_of_object_in_query_list(query_list = query_list)){
    stop("You are querying an object in an unexpected way. Please check {Vignette on objects} on how to query an object correctly.")
  }
  query_list <- set_query_list_class(query_list = query_list)
  query_list
}

sub_query <- function(...) {
  .list_dots <- list(...)
  query_list <- lapply(seq_along(.list_dots), \(i) {
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
  query_list
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

temper_query <- function(query_list, network) {
  class_input <- class(query_list)
  query_list <- sapply(query_list, \(link) {
    project <- link$from
    if (any(link$select == "*")) {
      link$select <- names(network[[project]]$variables)
    }
    link
  }, simplify = FALSE, USE.NAMES = TRUE)
  class(query_list) <- class_input
  query_list
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
  li_queries <- list(...)
  merged_query <- setNames(list(), character(0))
  for (query in li_queries) {
    projects <- vapply(query, \(q) q$from, FUN.VALUE = character(1))
    uq_projects <- unique(projects)
    positions <- lapply(uq_projects, \(proj) which(projects %in% proj))
    index <- seq_along(uq_projects)
    for(i in index) {
      proj <- uq_projects[i]
      pos <- positions[[i]]
      selection <- do.call(c, lapply(pos, \(p) query[[p]]$select))
      merged_query[[proj]]$select <- unique(c(merged_query[[proj]]$select, selection))
    }
  }
  all_projects <- names(merged_query)
  for (proj in all_projects) {
    merged_query[[proj]]$from <- proj
    class(merged_query[[proj]]) <- c("list", "xafty_query")
  }
  merged_query <- set_query_list_class(merged_query)
  merged_query
}

get_projects <- function(query) {
  vapply(query, \(q) q$from, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

get_joins_within_query <- function(query, network) {
  projects <- get_projects(query)
  # remove containers
  projects <- projects[vapply(projects, \(project) "xafty_project" %in% class(network[[project]]), FUN.VALUE = logical(1))]
  if(length(projects) <= 1) return(character(0))
  projects
}

dots_to_query <- function(network, ...)  {
  query_raw <- list(...)
  if(!inherits(query_raw[[1]], what = "xafty_query_list")) {
    query <- query(query_raw)
  } else {
    query <- list(...)[[1]]
  }
  query_order <- temper_query(query_list = query, network = network)
  query_internal <- merge_queries(query_order)

  list(
    internal = query_internal,
    order = query_order
  )
}

is_object_query_list <- function(query_list) {
  if (length(query_list) != 1) return(FALSE)
  if (!is_xafty_object_variable(query_list[[1]]$select)) return(FALSE)
  TRUE
}

set_query_list_class <- function(query_list) {
  if (is_object_query_list(query_list = query_list)) {
    class(query_list) <- c("list", "xafty_query_list", "xafty_object_query")
  } else {
    class(query_list) <- c("list", "xafty_query_list")
  }
  query_list
}

has_misuse_of_object_in_query_list <- function(query_list) {
  n_projects <- length(query_list)
  misuse_detected <- FALSE
  for (query in query_list) {
    project <- query$project
    select <- query$select
    object_variable_vec <- vapply(select, is_xafty_object_variable, FUN.VALUE = logical(1))
    has_object <- any(object_variable_vec)
    more_than_one <- length(object_variable_vec) > 1
    if((n_projects > 1 & has_object) | (has_object & more_than_one)) misuse_detected <- TRUE
  }
  misuse_detected
}
