
#' Register a Function in a Network
#' @description
#' The function is the workhorse behind all register methods of a network.
#' @param quosure A function call enquoted with rlang::enquo
#' @param project The project name of the project within the network where the function should be registered.
#' @param network A xafty network.
#' @param link_type The link_type name of the rulset. Currently "link" and "object" are supported
#' @param ... Configurations and advanced use when registering a new link
#' @returns A xafty network (invisibly)
#' @export
register <- function(quosure, project, network, link_type, ...) {
  link <- create_link(quosure = quosure, project = project, link_type = link_type, network = network, ... = ...)
  validate_network_integrity(link = link, network = network)
  add_to_settings(link = link, network = network, ... = ...)
  add_to_ruleset(link = link, network = network, ... = ...)
  add_to_network(link = link, project = project, network = network, ... = ...)
  invisible(network)
}

add_to_settings <- function(link, network, ...) {
  .dots <- list(...)
  df_projects <- network$settings$projects$print_order
  type <- link$type
  # Root Node registered
  if(type == "get") {
    project <- link$project
    node <- link$fun_name
    root_node <- df_projects$root[df_projects$project == project]
    if(is.na(root_node)) {
      df_projects$root[df_projects$project == project] <- node
    } else if (root_node != node) {
      update <- isTRUE(.dots[["update"]])
      if(!update) {
        warning(paste0("Project '",  project, "' already has a root node. Replacing current root node: '", root_node, "' with new root node: '", node, "'."))
      }
      df_projects$root[df_projects$project == project] <- node
    }
  }
  network$settings$projects$print_order <- df_projects
  invisible(network)
}

add_to_ruleset <- function(link, network, ...) {
  project <- link$project
  function_name <- link$fun_name
  .dots <- list(...)
  current_links <- network[[project]]$ruleset$nodes$links

  if(link_exists(link = link, network = network)) {
    update <- isTRUE(.dots[["update"]])
    if(!update) {
      user_update <- readline(paste0("Function '", function_name, "' was already registered in project '",
                                     paste0(project, collapse = " and "),"'. Would you like to update? (y/n): "))
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
      clean_variables <- current_links[[function_name]]$variables
      for (var in clean_variables) {
        network[[project]]$ruleset$nodes$variables[[var]] <- NULL
      }
      if(exists("user_update")) {
        message(paste0("Updated function '", function_name, "'!"))
      }
      # Add your update code here
    } else {
      # Abort the function
      message(paste0("Function '", link$fun_name, "' exists already in ruleset of project '",
                     paste0(project, collapse = " and "),"'"))
    }
  }
  network[[project]]$ruleset$nodes$links[[function_name]] <- link

  if(!is.null(.dots[["direction"]])) {
    if(.dots[["direction"]] == "both" & link$type == "join") {
      link$project <- get_join_project(link = link)
      add_to_ruleset(link = link, network = network, update = .dots[["update"]])
    }
  }

  network
}

link_exists <- function(link, network) {
  project <- link$project
  function_name <- link$fun_name
  links <- network[[project]]$ruleset$nodes$links
  exists <- function_name %in% names(links)
  exists
}

add_to_network <- function(link, project, network, ...) {
  .dots <- list(...)
  project <- link$project
  fun_name <- link$fun_name
  project_env <- network[[project]]
  group <- link$group

  if (inherits(link, "query_link")) {
    variables <- link$variables
    if(link$type == "get" || link$type == "add") {
      for (var in variables) {
        network[[project]]$ruleset$nodes$variables[[var]] <- fun_name
      }
    } else if(link$type == "join") {
      from <- link$project
      to <- get_join_project(link = link)
      if(length(variables) == 0) {
        # Here we assume the variables of the project are left joined
        exported <- names(network[[to]]$ruleset$nodes$variables)
      } else {
        exported <- NULL
      }
      network[[from]]$ruleset$nodes$joins[[to]] <- list(
        link = link,
        exported = exported
      )
      if(!is.null(.dots[["direction"]])) {
        if(.dots[["direction"]] == "both") {
          if(length(variables) == 0) {
            # Since the join was registered with no variables, we assume it must be an inner or full join
            exported <- names(network[[from]]$ruleset$nodes$variables)
          } else {
            exported <- NULL
          }
          link_to <- link
          link_to$project <- to
          network[[to]]$ruleset$nodes$joins[[from]] <- list(
            link = link_to,
            variables = exported
          )
        }
      }
    }
    if(!is.null(group)) {
      current_variables <- network[[project]]$ruleset$groups[[group]]$variables
      network[[project]]$ruleset$groups[[group]]$variables <- c(current_variables, variables)
    }
  }

  if(inherits(link, "context_link")) {
    context <- link$context
    if(link$type == "entry") {
      li_on_entry <- list(
        link = link
      )
      network[[project]]$ruleset$contexts[[context]]$on_entry[[fun_name]] <- li_on_entry
    } else if (link$type == "exit") {
      li_on_exit <- list(
        link = link
      )
      network[[project]]$ruleset$contexts[[context]]$on_exit[[fun_name]] <- li_on_exit
    }
  }
  invisible(network)
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
  link <- create_base_link(quosure = quosure, project = project, link_type = link_type,
                           group = .dots[["group"]], context = .dots[["attach_context"]])
  if (link_type == "query") {
    link <- link_add_variables(link = link, variable_names = .dots[["vars"]], network = network)
  } else if (link_type == "context") {
    link <- link_add_context(link = link, name = .dots[["name"]], func_type = .dots[["func_type"]])
  }
  link <- link_add_joins(link = link, network = network)
  link
}

create_base_link <- function(quosure, project, link_type = NULL, group, context) {
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
  link$type <- determine_link_type(args = list_args$args, link_type = link_type)
  link$context <- context
  link$group <- group
  link
}

link_add_context <- function(link, name, func_type) {
  # TODO: Need to warn the user when they create a polluted context
  if(is.null(name) || !is_valid_variable_name(match = name)) stop(paste0("Context name '", name,"' is not a valid variable name"))
  link$name <- paste0(name, ".", link$fun_name)
  link$type <- func_type
  link$context <- name
  class(link) <- c("list", "link", "context_link")
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
  link$layer <- determine_layer(link = link, network = network)
  link
}

link_add_joins <- function(link, network) {
  link$joins <- get_join_dependencies(link = link, network = network)
  link
}

#Get Dependent Joins for Each Argument
get_join_dependencies <- function(link, network) {
  queries_raw <- get_queries(link = link, which = "xafty_query", temper = TRUE, network = network)
  # Merging queries here for the edge case when a user duplicates the project, e.g. query(dupe = "col1", b = "col3", dupe = "col2")
  queries <- sapply(queries_raw, merge_queries, simplify = FALSE, USE.NAMES = TRUE)
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
  queries <- get_queries(link, temper = TRUE, network = network)
  if(length(queries) <= 0) return(invisible(TRUE))
  flat_queries <- flatten_list(queries)
  for (query in flat_queries) {
    project <- query$from
    selection <- query$select
    for (col in selection) {
      validate_query(name = col, project = project, network = network)
    }
  }
}

validate_query <- function(name, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- get_function_name(name = name, project = project, network = network)
  if(is.null(columns_subset)) {
    stop(paste0("Variable: ", name, " is not contained in project: ", project))
  }
  invisible(TRUE)
}

determine_link_type <- function(args, link_type) {
  if(is.null(link_type)) return(NULL)
  if(link_type == "query") {
    if(length(args) == 0) {
      n_queries <- 0
    } else {
      is_query_list <- vapply(args, \(arg) inherits(arg, "xafty_query_list"), FUN.VALUE = logical(1))
      n_queries <- sum(is_query_list)
    }
    if (n_queries == 0) {
      link_func_type <- "get"
    } else if (n_queries == 1) {
      link_func_type <- "add"
    } else if (n_queries == 2) {
      link_func_type <- "join"
    } else {
      stop("Only two projects may be joined per link")
    }
  } else {
    link_func_type <- NULL
  }
  link_func_type
}

determine_layer <- function(link, network) {
  type <- link$type
  if(type == "get") return(0)
  if(type == "add") {
    project <- link$project
    xafty_vec <- vapply(link$args, find_xafty_objects, FUN.VALUE = character(1))
    arg_name <- names(xafty_vec)[xafty_vec == "xafty_query"]
    states_list <- sapply(network$states, \(state) state$default, simplify = FALSE, USE.NAMES = TRUE)
    qry <- interpolate_state_in_query(merge_queries(link$args[[arg_name]]), state_list = states_list, network_env = network)
    is_project <- get_projects(qry) %in% project
    if(!any(is_project)) return(1) # impure non root
    ruleset <- network[[project]]$ruleset
    sub_qry <- qry[[which(is_project)]]
    funcs <- unique(vapply(sub_qry$select, \(name) get_function_name(name = name, project = project, network = network), FUN.VALUE = character(1)))
    layer <- max(vapply(funcs, \(fun) get_link(name = fun, project = project, network = network)$layer, FUN.VALUE = numeric(1))) + 1
  } else if (type == "join") {
    layer <- NULL
  }
  layer
}
