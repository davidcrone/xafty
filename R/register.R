
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
  link <- create_link(quosure = quosure, project = project, link_type = link_type, network = network, ... = ...)
  validate_network_integrity(link = link, network = network)
  add_to_ruleset(link = link, project = project, network = network, ... = ...)
  add_to_network(link = link, project = project, network = network, ... = ...)
  invisible(network)
}

add_to_network <- function(link, project, network, ...) {
  .dots <- list(...)
  project_env <- network[[project]]
  fun_name <- link$fun_name

  if (inherits(link, "query_link")) {
    variables <- link$variables
    added_joins <- get_ordered_join_pairs(link = link)
    for (new_join in added_joins) {
      from <-  new_join[1]
      to <- new_join[2]
      assign(to, fun_name, envir = network[[from]]$joined_projects)
    }
  } else if(inherits(link, "context_link") | inherits(link, "object_link")) {
    variables <- link$name
  }

  if(!is.null(.dots[["func_type"]])) {
    project <- link$project
    if(.dots[["func_type"]] == "entry") {
      current_entries <- network[[project]]$wrappers$on_entry
      new_entries <- c(current_entries, link$fun_name)
      network[[project]]$wrappers$on_entry <- new_entries
    } else if (.dots[["func_type"]] == "exit") {
      current_exits <- network[[project]]$wrappers$on_exit
      new_exits <- c(current_exits, link$fun_name)
      network[[project]]$wrappers$on_exit <- new_exits
    }
  }
  for (variable in variables) {
    assign(variable, fun_name, envir = project_env$variables)
  }
  invisible(network)
}

add_to_ruleset <- function(link, project, network,...) {
  function_name <- link$fun_name
  .dots <- list(...)
  # When registering an object or context, the object should only be registered in the project
  # The only register that should happen in two or more projects is when these projects are joined through a query link
  # TODO: This can be done more elegantly, by checking whether the user had the intention to join projects
  # and only if the join is symmetrical
  if(inherits(link, "query_link")) {
    projects <- unique(c(project, get_lead_projects(link)))
  } else {
    projects <- project
  }
  for (proj in projects) {
    current_rules <- network[[proj]]$ruleset
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
    network[[proj]]$ruleset[[function_name]] <- link
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

create_link <- function(quosure, project, link_type, network, ...) {
  .dots <- list(...)
  link <- create_base_link(quosure = quosure, project = project, link_type = link_type)
  if (link_type == "query") {
    link <- link_add_variables(link = link, variable_names = .dots[["vars"]], network = network)
  } else if (link_type == "context") {
    link <- link_add_context(link = link, name = .dots[["name"]])
  } else if (link_type == "object") {
    link <- link_add_object(link = link, name = .dots[["name"]])
  }
  link <- link_add_joins(link = link, network = network)
  link
}

create_base_link <- function(quosure, project, link_type = link_type) {
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_args$args <- unpack_query(args = list_args$args)
  list_info <- list(
    package = get_function_package(func_name = list_args$fun_name),
    language = "R",
    project = project
  )
  link <- append(list_args, list_info)
  class(link) <- c("list", "link")
  link
}

link_add_context <- function(link, name) {
  if(!is.null(name)) {
    if(!is_valid_variable_name(match = name)) stop(paste0("Context name '", name,"' is not a valid variable name"))
    link$name <- name
  } else {
    link$name <- NULL
  }
  class(link) <- c("list", "link", "context_link")
  link
}

link_add_object <- function(link, name = NULL) {
  if(!is.null(name)) {
    is_squared_already <- is_squared_variable(name)
    if(!is_squared_already) obj_name <- paste0("[", name, "]")
    if(!is_object_variable(obj_name)) stop(paste0("Object name '", name, "' is not a valid variable name."))
    link$name<- name
  } else {
    link$name <- NULL
  }
  class(link) <- c("list", "link", "object_link")
  link
}

link_add_variables <- function(link, variable_names = NULL, network) {
  if(!is.null(variable_names)) {
    link$variables <- variable_names
  } else {
    # Executes pipeline
    link$variables <- get_added_variables(link = link, network = network)
  }
  class(link) <- c("list", "link", "query_link")
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

unpack_query <- function(args) {
  sapply(args, \(arg) {
    if(inherits(arg, "xafty_query")) return(arg$query)
    arg
  }, simplify = FALSE, USE.NAMES = TRUE)
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
      col <- get_squared_variable(selection)
      validate_query(name = col, project = project, network = network)
    } else {
      for (col in selection) {
        validate_query(name = col, project = project, network = network)
      }
    }
  }
}

validate_query <- function(name, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset$variables[[name]]
  if(is.null(columns_subset)) {
    stop(paste0("Variable: ", name, " is not contained in project: ", project))
  }
  invisible(TRUE)
}
