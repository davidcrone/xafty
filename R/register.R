
#' Register a Function in a Network
#' @description
#' The function is the workhorse behind all register methods of a network.
#' @param quosure A function call enquoted with rlang::enquo
#' @param project The project name of the project within the network where the function should be registered.
#' @param network A xafty network.
#' @param link_type The link_type name of the rulset. Currently "link" and "object" are supported
#' @param ... Configurations and advanced use when registering a new link. Notable options:
#'   - `group` (character): Assign the resulting variables to an organizing group for printing purposes.
#'     If the group doesn't exist, it will be created automatically. Updating the node with group set
#'     to NULL will remove the variables from any previously assigned group.
#'   - `attach_context` (character): Attach the node to a context wrapper.
#'   - `update` (logical): Whether to update if the function is already registered.
#'   - `direction` (character): For joins, "one" or "both" for bidirectional registration.
#'   - `vars` (character vector): Explicitly specify output variable names.
#'   - `test_dag`(logical): Whether the newly registered node should be tested for cycles.
#' @returns A xafty network (invisibly)
#' @export
register <- function(quosure, project, network, link_type, ...) {
  state <- register_state_manager()
  link <- create_link(quosure = quosure, project = project, link_type = link_type, network = network, state = state, ... = ...)
  validate_network_integrity(link = link, network = network, ... = ...)
  add_to_settings(link = link, network = network, state = state, ... = ...)
  add_to_ruleset(link = link, network = network, state = state, ... = ...)
  add_to_network(link = link, network = network, ... = ...)
  validate_dag_integrity(link = link, network = network, state = state, ... = ...)
  invisible(network)
}

add_to_settings <- function(link, network, ...) {
  .dots <- list(...)
  state <- .dots[["state"]]
  state$set("settings", what = network$settings)
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
  if(!is_query_link(link)) return(invisible(network))
  project <- link$project
  function_name <- link$fun_name
  .dots <- list(...)
  state <- .dots[["state"]]
  state$set("update", FALSE)
  links <- network[[project]]$ruleset$nodes$links

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
      # Get previous link
      prev_link <- links[[function_name]]
      state$set("update", TRUE)
      state$set("prev_link", prev_link)

      # removes the previously registered link from the network
      remove_link(link = prev_link, network = network)
      # remove the previously registered variables
      remove_variables(link = prev_link, network = network)
      # Remove from group to update group
      remove_group(link = prev_link, network = network)
    } else {
      # Abort the register with the function, since nothing has been added to the network, we don't need to clean
      # anything up
      message(paste0("Function '", link$fun_name, "' exists already in ruleset of project '",
                     paste0(project, collapse = " and "),"'"))
    }
  }

  if(!is.null(.dots[["direction"]])) {
    if(.dots[["direction"]] == "both" & link$type == "join") {
      link$project <- get_join_project(link = link)
      add_to_ruleset(link = link, network = network, update = .dots[["update"]], direction = NULL, state = state)
    }
  }

  invisible(network)
}

link_exists <- function(link, network) {
  project <- link$project
  function_name <- link$fun_name
  links <- network[[project]]$ruleset$nodes$links
  exists <- function_name %in% names(links)
  exists
}

add_to_network <- function(link, network, ...) {
  .dots <- list(...)
  if (is_query_link(link)) {
    add_query_link(link = link, network = network, direction = .dots[["direction"]])
  }
  if(is_context_link(link)) {
    add_context_link(link = link, network = network)
  }
  add_group(link = link, network = network)
  invisible(network)
}

update_print_join_graph <- function(changed, network) {
  affected <- get_affect_projects_by_join(changed = changed, network = network)
  updates <- c(changed, affected)
  for (project in updates) {
    centered_graph <- build_join_graph(main_project = project, network = network)
    network[[project]]$ruleset$graph <- build_exported_variables(project = project, graph = centered_graph, network = network)
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
  link <- create_base_link(quosure = quosure, project = project, link_type = link_type,
                           group = .dots[["group"]], context = .dots[["attach_context"]])
  raw_args <- link$args
  states <- build_states(states = list(), network = network)
  link <- interpolate_link_queries(link = link, states = states)
  if (link_type == "query") {
    link <- link_add_variables(link = link, variable_names = .dots[["vars"]], network = network)
  } else if (link_type == "context") {
    link <- link_add_context(link = link, name = .dots[["name"]], func_type = .dots[["func_type"]])
  }
  link$args <- raw_args
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

validate_network_integrity <- function(link, network, ...) {
  check_variable_duplicates(link = link, network = network)
  check_context_presence(link = link, network = network)
  check_query_presence(link = link, network = network)
}

validate_dag_integrity <- function(link, network, ...) {
  .dots <- list(...)
  test_dag <- .dots[["test_dag"]]
  if(test_dag) {
    dag <- detect_cycle(link = link, network = network)
    if(isFALSE(dag)) {
      revert_register(link = link, network = network, ... = ...)
    } else {
      # Check for polluted context
      check_polluted_context(link = link, network = network)
    }
  }
  invisible(network)
}

check_context_presence <- function(link, network) {
  # Only check when node is a query link and has a context attached to it
  if(length(link$context) == 0 || is_context_link(link)) return(invisible(TRUE))
  project <- link$project
  context_name <- link$context
  context <- network[[project]]$ruleset$contexts[[context_name]]
  if(is.null(context)) warning(paste0("Node ", link$fun_name, " was attached to context '", context_name,
                                   "' but context '", context_name, "' has not yet been created in project '", link$project, "'"))
  invisible(TRUE)
}

check_query_presence <- function(link, network) {
  queries <- get_queries(link, temper = TRUE, states = build_states(states = list(), network = network))
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

check_variable_duplicates <- function(link, network) {
  if(length(link$variables) == 0) return(invisible(TRUE))
  project <- link$project
  added <- link$variables
  current <- names(network[[project]]$ruleset$nodes$variables)
  exist <- added[added %in% current]
  if(length(exist) == 0) return(invisible(TRUE))
  fun_exist <- vapply(exist, \(name) get_function_name(name = name, project = project, network = network), FUN.VALUE = character(1))
  fun_added <- link$fun_name
  duplicates <- fun_exist[!fun_exist == fun_added]
  if(length(duplicates) > 0) {
    vec_error <-  paste0("Variable: '", names(duplicates), "' is already added through node: '", duplicates, "'")
    error <- paste0(vec_error, collapse = "\n  ")
    stop(error)
  }
  invisible(TRUE)
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

check_polluted_context <- function(link, network) {
  if(length(link$context) <= 0) return(invisible(TRUE))
  project <- link$project
  context <- link$context
  if(link$type == "exit") {
    exit_query <- get_queries(link)
  } else {
    exit_link <- network[[project]]$ruleset$contexts[[context]]$on_exit$link
    if(is.null(exit_link)) return(invisible(TRUE))
    exit_query <- get_queries(exit_link)
  }
  if(length(exit_query) == 0) return(invisible(TRUE))
  exit_node <- build_fun_code(link)
  dag <- build_dag(exit_query[[1]], network)
  order <- dag$execution_order
  prefix <- paste0(build_namespace(link), ".")
  is_project <- which(startsWith(order, prefix))
  if(length(is_project) == 0) return(invisible(TRUE))
  start_pos <- min(is_project)
  end_pos <- max(is_project)
  projects_extract <- order[start_pos:end_pos]
  li_classified <- classify_foreign_dependencies(project = project,
                                                 group = context,
                                                 dag = dag$dag[projects_extract],
                                                 targets = exit_node,
                                                 contexts = NULL,
                                                 stop_at = character(0),
                                                 network = network)
  if(length(li_classified$pollution)) warning(paste0("Created polluted context for ", context)) else return(invisible(TRUE))
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
    qry <- merge_queries(link$args[[arg_name]])
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

remove_variables <- function(link, network) {
  project <- link$project
  vars <- link$variables
  for (var in vars) {
    network[[project]]$ruleset$nodes$variables[[var]] <- NULL
  }
  invisible(network)
}

remove_group <- function(link, network) {
  project <- link$project
  group <- link$group
  vars <- link$variables
  if(!is.null(group)) {
    group_vars <- network[[project]]$ruleset$groups[[group]]$variables
    remaining <- group_vars[!group_vars %in% vars]
    if(length(remaining) != 0) {
      network[[project]]$ruleset$groups[[group]]$variables <- remaining
    } else {
      # remove group entirely if no variables remain within the group anymore
      network[[project]]$ruleset$groups[[group]] <- NULL
    }
  }
  invisible(network)
}

remove_link <- function(link, network) {
  project <- link$project
  fun_name <- link$fun_name
  network[[project]]$ruleset$nodes$links[[fun_name]] <- NULL
  network
}

add_query_link <- function(link, network, direction) {
  project <- link$project
  fun_name <- link$fun_name
  variables <- link$variables
  type <- link$type
  if(type == "get" || type == "add") {
    for (var in variables) {
      network[[project]]$ruleset$nodes$variables[[var]] <- fun_name
    }
    # Update the project's ruleset with the new link
    network[[project]]$ruleset$nodes$links[[fun_name]] <- link
  } else if(type == "join") {
    from <- link$project
    to <- get_join_project(link = link)
    if(length(variables) == 0) {
      # Here we assume the variables of the project are left joined
      exported <- names(network[[to]]$ruleset$nodes$variables)
    } else {
      exported <- NULL
    }
    joins_to <- names(network[[to]]$ruleset$nodes$joins)

    network[[from]]$ruleset$nodes$joins[[to]] <- list(link = link)
    network <- update_print_join_graph(changed = from, network = network)
    if(!is.null(direction)) {
      if(direction == "both") {
        if(length(variables) == 0) {
          # Since the join was registered with no variables, we assume it must be an inner or full join
          exported <- names(network[[from]]$ruleset$nodes$variables)
        } else {
          exported <- NULL
        }
        link_to <- link
        link_to$project <- to
        network[[to]]$ruleset$nodes$joins[[from]] <- list(link = link_to)
        network <- update_print_join_graph(changed = to, network = network)
      }
    }
  }
  invisible(network)
}

add_context_link <- function(link, network) {
  project <- link$project
  fun_name <- link$fun_name
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
  invisible(network)
}

add_group <- function(link, network) {
  group <- link$group
  if(!is.null(group)) {
    variables <- link$variables
    project <- link$project
    # Auto-create group if it doesn't exist
    if (is.null(network[[project]]$ruleset$groups[[group]])) {
      network[[project]]$ruleset$groups[[group]] <- list(variables = NULL)
    }
    current_variables <- network[[project]]$ruleset$groups[[group]]$variables
    network[[project]]$ruleset$groups[[group]]$variables <- c(current_variables, variables)
  }
  invisible(network)
}

build_testable_query <- function(link, network) {
  project <- link$project
  qu <- list()
  # links of type get cannot be a cycle
  if (link$type == "add") {
    vars <- link$variables
    el <- setNames(list(vars), project)
    qu <- do.call(query, el)
    qu <- do.call(from, list(qu, project = project))
  } else if (link$type == "join") {
    qu <- do.call(merge_queries, get_queries(link))
    qu <- from(qu, project = project)
  }
  qu
}

# TODO: Build new function that checks the dag for a cycle returning TRUE/FALSE instead of an error upon finding a cycle
detect_cycle <- function(link, network) {
  test_query <- build_testable_query(link = link, network = network)
  if(!length(test_query) == 0) {
    dag <- tryCatch({
      build_dag(query = test_query, network = network)
    }, error = \(e) {
      message(e)
      return(FALSE)
      }
    )
  } else (
    dag <- list()
  )
  dag
}

## TODO: Adding remove node from Project and other remove functions build in as methods in the project object
revert_register <- function(link, network, ...) {
  .dots <- list(...)
  # removes the previously registered link from the network
  remove_link(link = link, network = network)
  # remove the previously registered variables
  remove_variables(link = link, network = network)
  # Remove from group to update group
  remove_group(link = link, network = network)

  state <- .dots[["state"]]
  prev_link <- state$get("prev_link")
  if(!is.null(prev_link)) {
    add_to_network(link = prev_link, network = network, ... = ...)
  }
  cat("Register was rejected")
  invisible(network)
}
