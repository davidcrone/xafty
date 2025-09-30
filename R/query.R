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

#' Add a State to a xafty Query
#' @description
#' The state of a xafty query is passed to arguments that were declared as xafty states
#' by passing a single character vector wrapped into {curley_braces} into the argument
#' @param query_list A query list created by [xafty::query]
#' @param ... Declaration of the states e.g. state_name = TRUE
#' @returns A list state_query which is a query bundled with a state
#' @export
with <- function(query_list, ...) {
  .li_states <- list(...)
  class(.li_states) <- c("list", "xafty_states_list")
  state_query <- list(
    query = query_list,
    states = .li_states
    )
  class(state_query) <- c("list", "state_query")
  state_query
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

temper_query <- function(query_list, state_list = NULL, network) {
  class_input <- class(query_list)
  query_list <- resolve_star_select(query_list = query_list, network_env = network)
  query_list <- interpolate_state_in_query(query_list = query_list, state_list = state_list, network_env = network)
  class(query_list) <- class_input
  query_list
}

get_sub_queries <- function(query, network) {
  projecs <- names(query)
  sub_query_list <- lapply(projecs, \(project) {
    project_env <- network[[project]]
  })
  sub_query_list[!vapply(sub_query_list, \(query) is.null(query), FUN.VALUE = logical(1))]
}

#' @importFrom stats setNames
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
  # When only the network is provided, the user gets the following error
  # However only providing the network could also be used as a shorthand to query all projects with all columns within the network
  if(length(query_raw) == 0) {
    stop("No query provided. Please pass a valid query into the function.")
  }
  if(inherits(query_raw[[1]], "state_query")) {
    query_list <- query_raw[[1]]$query
    state_list <- query_raw[[1]]$states
  } else if (inherits(query_raw, "state_query")) {
    query_list <- query_raw$query
    state_list <- query_raw$states
  } else if (inherits(query_raw[[1]], what = "xafty_query_list")) {
    query_list <- query_raw[[1]]
    state_list <- NULL
  } else if(!inherits(query_raw[[1]], what = "xafty_query_list")) {
    query_list <- query(query_raw)
    state_list <- NULL
  } else {
    stop("Could not parse passed query")
  }
  query_order <- temper_query(query_list = query_list, state_list = state_list, network = network)
  query_internal <- merge_queries(query_order)
  list(
    internal = query_internal,
    order = query_order,
    states = state_list
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

resolve_star_select <- function(query_list, network_env) {
  sapply(query_list, \(query) {
    project <- query$from
    if (any(query$select == "*")) {
      query$select <- names(network_env[[project]]$variables)
    }
    query
  }, simplify = FALSE, USE.NAMES = TRUE)
}

interpolate_state_in_query <- function(query_list, state_list, network_env) {
  for (query in query_list) {
    select <- query$select
    project <- query$from
    contains_state_logical <- vapply(select, contains_state, FUN.VALUE = logical(1), USE.NAMES = FALSE)
    if(!any(contains_state_logical)) next
    position_states <- which(contains_state_logical)
    state_names <- get_braced_variable(select[contains_state_logical])
    for (i in seq_along(state_names)) {
      name <- state_names[i]
      inter_value <- state_list[[name]]
      if(is.null(inter_value)) {
        inter_value <-  get_default_state(name = name, network_env = network_env)
        if(is.null(inter_value)) inter_value <- ""
      }
      pos <- position_states[i]
      variable_name <- select[pos]
      state_pattern <- paste0("{", name, "}")
      inter_variable <- gsub(state_pattern, inter_value, variable_name, fixed = TRUE)
      query_list[[project]]$select[pos] <- inter_variable
    }
  }
  query_list
}
