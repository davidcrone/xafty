
#' Register a Function in a Network
#' @description
#' The function is the workhorse behind all register methods of a network.
#' @param quosure A function call enquoted with rlang::enquo
#' @param project The project name of the project within the network where the function should be registered.
#' @param network A xafty network.
#' @param link_type The link_type name of the rulset. Currently "link" and "object" are supported
#' @param ... Unused. Configurations of the link type.
#' @returns A xafty network (invisibly)
#' @export
register <- function(quosure, project, network, link_type, ...) {
  link <- create_link(quosure = quosure,  project = project, network = network, ... = ...)
  validate_network_integrity(link = link, network = network)
  add_to_ruleset(link = link, link_type = link_type, network = network, project = project, ... = ...)
  add_to_network(link = link, network = network, project = project, ... = ...)
  invisible(network)
}

add_to_network <- function(link, network, project, ...) {
  .dots <- list(...)
  project_env <- network[[project]]
  fun_name <- link$fun_name

  if(!is.null(.dots[["object_name"]])) {
    added_object <- get_squared_variable(link$added_object)
    assign(added_object, fun_name, envir = project_env$objects)
  }

  if(!is.null(.dots[["context_name"]])) {
    context_name <- link$name
    assign(context_name, fun_name, envir = project_env$context)
  }

  variables <- link$variables
  for (new_col in variables) {
    assign(new_col, fun_name, envir = project_env$variables)
  }
  added_joins <- get_ordered_join_pairs(link = link)
  for (new_join in added_joins) {
    from <-  new_join[1]
    to <- new_join[2]
    assign(to, fun_name, envir = network[[from]]$joined_projects)
  }
  invisible(network)
}

add_to_ruleset <- function(link, link_type = "link", network, project, ...) {
  function_name <- link$fun_name
  .dots <- list(...)
  # When registering an object or context, the object should only be registered in the project
  # The only register that should happen in two or more projects is when these projects are joined
  # TODO: This can be done more elegantly, by checking whether the user had the intention to join projects
  # and only if the join is symmetrical
  if(is.null(.dots[["object_name"]]) | is.null(.dots[["context_name"]])) {
    projects <- unique(c(project, get_lead_projects(link)))
  } else {
    projects <- project
  }
  new_rule <- list(link)
  new_rule <- setNames(new_rule, function_name)
  for (proj in projects) {
    current_rules <- network[[proj]]$ruleset[[link_type]]
    if(function_name %in% names(current_rules)) {
      update <- isTRUE(.dots[["update"]])
      if(!update) {
        user_update <- readline(paste0("Function '", function_name, "' was already registered in project '",  paste0(projects, collapse = " and "),"'. Would you like to update? (y/n): "))
        if(user_update == "y") {
          update <- TRUE
        } else {
          update <- FALSE
        }
      }
      # Check the user input
      if (update) {
        # Proceed with the update
        # remove the previously registered variables
        clean_variables <- current_rules[[function_name]]$variables
        rm(list = clean_variables, envir = network[[project]]$variables)
        current_rules[[function_name]] <- NULL
        if(exists("user_update")) {
          message(paste0("Updated function '", function_name, "'!"))
        }
        # Add your update code here
      } else {
        # Abort the function
        message(paste0("Function '", link$fun_name, "' exists already in ruleset of project '",
                       paste0(projects, collapse = " and "),"'"))
      }
    }
    add_rules <- c(current_rules, new_rule)
    network[[proj]]$ruleset[[link_type]] <- add_rules
  }
  network
}

#' Get the Package Name of a Function
#'
#' This function determines the package where a given function is defined.
#'
#' @param func_name A character string specifying the name of the function.
#' @return A character vector of package names where the function is found, or `NA` if the function is not found in any package.
#' @importFrom utils combn getAnywhere
get_function_package <- function(func_name) {
  # Validate input
  if (!is.character(func_name) || length(func_name) != 1) {
    stop("The function name must be a single string.")
  }

  # Get function details
  func_details <- getAnywhere(func_name)
  if (length(func_details$where) > 0) {
    # Remove "<environment>" entries and keep only package names
    if(".GlobalEnv" %in% func_details$where) return(".GlobalEnv")
    packages <- unique(grep("^package:", func_details$where, value = TRUE))

    if (length(packages) > 0) {
      return(sub("^package:", "", packages))  # Remove "package:" prefix
    }
  }

  return(NA)  # Return NA if no package is found
}

create_link <- function(quosure, project, network, ...) {
  .dots <- list(...)
  link <- create_base_link(quosure = quosure, project = project)
  link <- link_add_variables(link = link, variable_names = .dots[["vars"]], network = network)
  link <- link_add_context(link = link, context_name = .dots[["context_name"]])
  link <- link_add_object(link = link, object_name = .dots[["object_name"]])
  link <- link_add_joins(link = link, network = network)
  link
}

create_base_link <- function(quosure, project) {
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_info <- list(
    package = get_function_package(func_name = list_args$fun_name),
    language = "R",
    project = project
  )
  link <- append(list_args, list_info)
  class(link) <- c("xafty_link", "list")
  link
}

link_add_context <- function(link, context_name) {
  if(!is.null(context_name)) {
    if(!is_valid_variable_name(match = context_name)) stop("Name of context is not a valid variable name")
    link$name <- context_name
  } else {
    link$name <- NULL
  }
  link
}

link_add_object <- function(link, object_name = NULL) {
  if(!is.null(object_name)) {
    is_squared_already <- is_squared_variable(object_name)
    if(!is_squared_already) object_name <- paste0("[", object_name, "]")
    if(!is_object_variable(object_name)) stop("object_name is not a valid xafty object variable")
    link$added_object <- object_name
  } else {
    link$added_object <- NULL
  }
  link
}

link_add_variables <- function(link, variable_names = NULL, network) {
  if(!is.null(variable_names)) {
    link$variables <- variable_names
  } else {
    # Executes pipeline
    link$variables <- get_added_variables(link = link, network = network)
  }
  link
}

link_add_joins <- function(link, network) {
  link$joins <- get_join_dependencies(link = link, network = network)
  link
}

#Get Dependent Joins for Each Argument
get_join_dependencies <- function(link, network) {
  queries <- get_queries(link = link, which = "xafty_query", temper = TRUE, network = network)
  args <- names(queries)
  list_projects <- sapply(queries, get_projects, simplify = FALSE, USE.NAMES = TRUE)
  list_join_projects <- sapply(args, \(arg) {
    query_list <- queries[[arg]]
    projects <- list_projects[[arg]]
    logical_vec <- vapply(projects, \(project) project_needs_join(project = project, query_list = query_list, network = network),
                          FUN.VALUE = logical(1), USE.NAMES = TRUE)
    projects[logical_vec]
  }, simplify = FALSE, USE.NAMES = TRUE)
  is_one_project <- vapply(list_join_projects, \(projects) length(projects) <= 1, FUN.VALUE = logical(1))
  list_projects <- list_join_projects[!is_one_project]
  list(
    projects = list_projects
  )
}

unpack_args <- function(exp, env) {
  fun_name <- as.character(exp[[1]])
  fun <- get(fun_name, envir = env)
  fun_args <- as.list(formals(fun))
  matched_call <- match.call(definition = fun, call = exp, envir = env, expand.dots = FALSE)
  for (arg_name in names(fun_args)) {
    if (!(arg_name %in% names(matched_call))) {
      # Case 1: Default argument
      fun_args[[arg_name]] <- rlang::eval_tidy(fun_args[[arg_name]], env = env)
    } else {
      if(arg_name == "...") {
        fun_args <- handle_dots_args(fun_args, matched_call, env = env)
      } else {
        call_expr <- matched_call[[arg_name]]
        fun_args[[arg_name]] <- rlang::eval_tidy(call_expr, env =  env)
      }
    }
  }
  list(
    fun_name = fun_name,
    fun = fun,
    args = fun_args
  )
}

validate_network_integrity <- function(link, network) {
  # TODO Needs also to work with a state_query (query bundled with state)
  queries <- get_queries(link, temper = TRUE, network = network)
  if(length(queries) <= 0) return(invisible(TRUE))
  flat_queries <- flatten_list(queries)
  for (query in flat_queries) {
    project <- query$from
    selection <- query$select
    if (is_object_variable(selection)) {
      validate_query(col = get_squared_variable(selection), project = project, network = network, env_name = "objects")
    } else {
      for (col in selection) {
        validate_query(col = col, project = project, network = network, env_name = "variables")
      }
    }
  }
}

validate_query <- function(col, project, network, env_name = "variables") {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset[[env_name]][[col]]
  if(is.null(columns_subset)) {
    stop(paste0("Column: ", col, " is not contained in project: ", project))
  }
  invisible(TRUE)
}
