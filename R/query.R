#' Build a xafty Query
#' @description
#' A xafty query will be passed to the nascent function in order to retrieve data from the network
#' @param ... Query value. See examples for creating a simple query
#' @examples
#' query(project_name1 = c("col1", "col2"), project_name2 = c("colA"))
#' # Tidy-Style selection is also supported
#' query(var1)
#' # Named vector style for renaming is also supported
#' query(table1 = c("Rename" = "var1"), var2)
#' # Nested list style is also supported
#' query(list(customer_data = "id"))
#' @export
query <- function(...) {

  # Capture all arguments as expressions without evaluating
  all_exprs <- rlang::exprs(...)

  # flatten any list(...) expressions into their elements ---
  all_exprs <- flatten_list_exprs(all_exprs, env = parent.frame())

  li_query_raw <- list()

  for (i in seq_along(all_exprs)) {
    arg_expr <- all_exprs[[i]]
    arg_name <- names(all_exprs)[[i]]
    has_table <- !is.null(arg_name) && arg_name != ""

    if (has_table) {
      # Named argument — evaluate to get the vector (preserving names)
      arg_val <- eval(arg_expr, envir = parent.frame())
      entry <- list(arg_val)
      names(entry) <- arg_name
    } else {
      # Bare symbol — tidy-style, convert to string without evaluating
      arg_val <- rlang::as_string(arg_expr)
      entry <- list(arg_val)
      names(entry) <- NULL
    }

    li_query_raw <- append(li_query_raw, entry)
  }

  # Build the query list from li_query_raw
  query_list <- lapply(seq_along(li_query_raw), \(i) {
    li_query   <- li_query_raw[i]
    select_raw <- li_query_raw[[i]]
    rename     <- names(select_raw)
    select     <- unname(select_raw)
    project    <- names(li_query)

    if (is.null(project) || project == "") {
      xafty_query <- list(
        select = select,
        from   = "unevaluated"
      )
      class(xafty_query) <- c("list", "raw_query")
    } else {
      xafty_query <- list(
        select = select,
        from   = project
      )
      # Attach new elements here. Keeps them, if they are not NULL
      xafty_query$rename <- rename
      class(xafty_query) <- c("list", "xafty_query")
    }
    xafty_query
  })

  names(query_list) <- vapply(query_list, \(query) query$from, FUN.VALUE = character(1))
  class(query_list) <- c("list", "xafty_query_list")
  query_list
}


#' Flatten list-producing call expressions into their constituent named expressions
#'
#' Handles three cases:
#'   1. Literal list() calls           → evaluate and flatten
#'   2. Other calls returning named lists → evaluate and flatten
#'   3. Bare symbols (tidy-style)       → pass through unchanged
#'
#' @param exprs A named list of expressions as returned by rlang::exprs()
#' @param env The environment in which to evaluate calls
#' @return A flat named list of expressions with any list-producing calls unpacked
#' @noRd
flatten_list_exprs <- function(exprs, env) {
  result <- list()

  for (i in seq_along(exprs)) {
    expr      <- exprs[[i]]
    expr_name <- names(exprs)[[i]]
    has_name  <- !is.null(expr_name) && nzchar(expr_name)

    # A bare symbol with no argument name = tidy-style selection, never evaluate
    is_bare_symbol <- rlang::is_symbol(expr) && !has_name

    if (is_bare_symbol) {
      # Pass through for tidy-style handling in the main loop
      entry        <- list(expr)
      names(entry) <- expr_name
      result       <- c(result, entry)

    } else if (rlang::is_call(expr) && !has_name) {
      # An unnamed call — evaluate and check if it returns a named list
      evaled <- tryCatch(
        eval(expr, envir = env),
        error = \(e) NULL
      )

      if (is.list(evaled) && !is.null(names(evaled))) {
        # Named list returned — flatten into individual named expressions
        inner_exprs        <- lapply(evaled, \(val) rlang::expr(!!val))
        names(inner_exprs) <- names(evaled)
        result             <- c(result, inner_exprs)
      } else {
        # Not a named list — pass through and let the main loop error informatively
        entry        <- list(expr)
        names(entry) <- expr_name
        result       <- c(result, entry)
      }

    } else {
      # Named argument (named vector, renamed vector etc.) — pass through unchanged
      entry        <- list(expr)
      names(entry) <- expr_name
      result       <- c(result, entry)
    }
  }

  result
}

#' Specify the Main Project for a Query
#' @param query_list A query object returned from query()
#' @param project Name of the project
#' @export
#' @returns A xafty Query
from <- function(query_list, project) {
  project <- deparse(substitute(project))
  if(length(project) != 1) stop("Project must be of length 1")
  if(inherits(query_list, "state_query")) {
    state_query <- add_to_state_query(name = "main", what = project, state_query = query_list)
  } else {
    state_query <- list(
      query = query_list,
      main = project
    )
  }
  class(state_query) <- c("list", "xafty_query")
  state_query
}

#' Add a State to a xafty Query
#' @description
#' The state of a xafty query is passed to arguments that were declared as xafty states
#' by passing a single character vector wrapped into \code{{curley_braces}} into the argument
#' @param query_list A query list created by [xafty::query]
#' @param ... Declaration of the states e.g. state_name = TRUE
#' @returns A list state_query which is a query bundled with a state
#' @export
with_state <- function(query_list, ...) {
  .li_states <- list(...)
  class(.li_states) <- c("list", "xafty_states_list")
  if(inherits(query_list, "state_query")) {
    state_query <- add_to_state_query(name = "states", what = .li_states, state_query = query_list)
  } else {
    state_query <- list(
      query = query_list,
      states = .li_states
    )
  }
  class(state_query) <- c("list", "xafty_query")
  state_query
}

#' Add a Predefined Join Path to a Query
#' @description
#' The function gives control over the desired joins in order to bring all projects together into one data.frame. The joins are
#' read left-to-right. An example can be found under examples on how to structure the list.
#' @param query_list A xafty query list returned by query()
#' @param ... List of the join path
#' @examples
#' \dontrun{
#' qry <- query(projectA = "col1", projectB = "col2", projectC = "col3")
#' add_join_path(qry, path1 = c("projectA", "projectB"), path2 = c("projectA", "projectC"))
#' # this creates the following joins: projectA joins with projectB and projectA joins with projectC
#' }
#' @returns A list
#' @export
add_join_path <- function(query_list, ...) {
  .li_states <- list(...)
  class(.li_states) <- c("list", "xafty_join_path")

  if(inherits(query_list, "state_query")) {
    state_query <- add_to_state_query(name = "join_path", what = .li_states, state_query = query_list)
  } else {
    state_query <- list(
      query = query_list,
      join_path = .li_states
    )
  }
  class(state_query) <- c("list", "xafty_query")
  state_query
}

add_to_state_query <- function(name, what, state_query) {
  state_query[[name]] <- what
  state_query
}

temper_query <- function(query_list, states = NULL, network) {
  class_input <- class(query_list)
  query_list <- fill_raw_query(query_list = query_list, network = network)
  query_list <- resolve_star_select(query_list = query_list, network_env = network)
  query_list <- interpolate_state_in_query(query_list = query_list, states = states)
  class(query_list) <- class_input
  query_list
}

#' @importFrom stats setNames
merge_queries <- function(...) {
  .list_queries <- list(...)
  merged_query <- setNames(list(), character(0))
  for (query in .list_queries) {
    projects <- vapply(query, \(q) q$from, FUN.VALUE = character(1))
    uq_projects <- unique(projects)
    positions <- lapply(uq_projects, \(proj) which(projects %in% proj))
    index <- seq_along(uq_projects)
    for(i in index) {
      proj <- uq_projects[i]
      pos <- positions[[i]]
      selection <- unlist(lapply(pos, \(p) query[[p]]$select))
      rename <- unlist(lapply(pos, \(p) if(is.null(query[[p]]$rename)) character(length(query[[p]]$select)) else query[[p]]$rename))
      merged_query[[proj]]$select <- unique(c(merged_query[[proj]]$select, selection))
      merged_query[[proj]]$rename <- unique(c(merged_query[[proj]]$rename, rename))
    }
  }
  all_projects <- names(merged_query)
  for (proj in all_projects) {
    merged_query[[proj]]$from <- proj
    merged_query[[proj]] <- merged_query[[proj]][c("select", "from", "rename")]
    if(all(merged_query[[proj]]$rename == "")) merged_query[[proj]]$rename <- NULL
    class(merged_query[[proj]]) <- c("list", "xafty_query")
  }
  class(merged_query) <- c("list", "xafty_query_list")
  merged_query
}

get_projects <- function(query) {
  vapply(query, \(q) q$from, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

get_join_projects <- function(query_list) {
  li_projects <- lapply(query_list, get_projects)
  li_unique <- lapply(li_projects, unique)
  is_one <- vapply(li_unique, \(projs) length(projs) <= 1, FUN.VALUE = logical(1))
  li_unique[!is_one]
}

dots_to_query <- function(network, ...)  {
  query_raw <- list(...)
  # When only the network is provided, the user gets the following error
  # However only providing the network could also be used as a shorthand to query all projects with all columns within the network
  if(length(query_raw) == 0) {
    stop("No query provided. Please pass a valid query into the function.")
  }
  if(inherits(query_raw[[1]], "xafty_query")) {
    query_list <- query_raw[[1]]$query
    state_list <- query_raw[[1]]$states
    join_path <- query_raw[[1]]$join_path
    main <- query_raw[[1]]$main
  } else if (inherits(query_raw, "xafty_query")) {
    query_list <- query_raw$query
    state_list <- query_raw$states
    join_path <- query_raw$join_path
    main <- query_raw[[1]]$main
  } else if (inherits(query_raw[[1]], what = "xafty_query_list")) {
    query_list <- query_raw[[1]]
    state_list <- NULL
    join_path <- NULL
    main <- NULL
  } else if(!inherits(query_raw[[1]], what = "xafty_query_list")) {
    query_list <- query(query_raw)
    state_list <- NULL
    join_path <- NULL
    main <- NULL
  } else {
    stop("Could not parse passed query")
  }
  states <- build_states(states = state_list, network = network)

  query_tempered <- temper_query(query_list = query_list, states = states, network = network)
  main <- if(is.null(main)) get_lead_project(query_tempered) else main
  query_order <- remove_where_query(query_tempered)
  query_internal <- merge_queries(remove_where_expr(query_tempered))

  list(
    main = main,
    internal = query_internal,
    order = query_order,
    states = states,
    join_path = join_path,
    where = get_where(query_tempered)
  )
}

resolve_star_select <- function(query_list, network_env) {
  sapply(query_list, \(query) {
    project <- query$from
    if (any(query$select == "*")) {
      variables <- names(get_all_variables(project = project, network = network_env))
      if(length(variables) == 0) stop(paste0("No variables found in project: '", project, "'!"))
      select <- query(setNames(list(variables), query$from))
      query$select <- variables
    }
    query
  }, simplify = FALSE, USE.NAMES = TRUE)
}

interpolate_state_in_query <- function(query_list, states) {
  for (i in seq_along(query_list)) {
    query <- query_list[[i]]
    select <- query$select
    available <- names(states)
    contains_state_logical <- vapply(select, contains_state, FUN.VALUE = logical(1), USE.NAMES = FALSE)
    if(!any(contains_state_logical)) next
    position_states <- which(contains_state_logical)
    state_names <- get_braced_variable(select[contains_state_logical])
    for (j in seq_along(state_names)) {
      name <- state_names[j]

      if(name %in% available) {
        inter_value <- states[[name]]
      } else {
        inter_value <- states$xafty_global_default
      }
      if(is.null(inter_value)) inter_value <- ""
      pos <- position_states[j]
      variable_name <- select[pos]
      state_pattern <- paste0("{", name, "}")
      inter_variable <- gsub(state_pattern, inter_value, variable_name, fixed = TRUE)
      query_list[[i]]$select[pos] <- inter_variable
    }
  }
  query_list
}

fill_raw_query <- function(query_list, network) {
  is_raw_query <- vapply(query_list, \(query) inherits(query, "raw_query"), logical(1))
  if(!any(is_raw_query)) return(query_list)
  is_project <- vapply(names(network), \(name) inherits(network[[name]], "xafty_project"), FUN.VALUE = logical(1))
  projects <- names(network)[is_project]
  query_list <- sapply(query_list, \(query) {
    if(inherits(query, "raw_query")) {
      variables <- query$select
      # Case 1: Querying an entire project by name
      if(length(variables) == 1 && any(variables %in% projects)) {
        query$from <-  variables
        query$select <- names(get_all_variables(project = query$from, network = network))
        query_classes <- class(query)
        query_classes[query_classes == "raw_query"] <- "xafty_query"
        class(query) <- query_classes
        return(query)
      }
      # Case 2: Querying single variables
      for (project in projects) {
        all_variables <- names(get_all_variables(project = project, network = network))
        has_variable <- any(variables %in% all_variables)
        if(has_variable) {
          query$from <-  project
          query_classes <- class(query)
          query_classes[query_classes == "raw_query"] <- "xafty_query"
          class(query) <- query_classes
        }
      }
    }
    query
  }, simplify = FALSE, USE.NAMES = TRUE)
  class(query_list) <- c("list", "xafty_query_list")
  query_list
}
