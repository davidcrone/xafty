#' Initialize a New xafty Network
#' @description
#' Use this function to initialize a new xafty network.
#' @param name Character vector of length 1. A valid name for the network
#' @param projects Character vector of project names that should be added to the network
#' @returns A 'xafty_network' environment
#' @examples
#' {
#' # Initialize the network
#' new_network <- init_network(name = "network_1")
#'
#' # Add a new project
#' new_network$add_project("project1")
#' }
#' @export
init_network <- function(name, projects = NULL) {
  network <- new.env() # This is the network where all projects will be merged together
  network$settings <- settings(name = name)
  network$states <- list()
  add_project <- create_add_project(network = network)
  save_project <- create_save_project(network = network)
  add_state <- create_add_state(network = network)
  assign("add_project", add_project, envir = network)
  assign("save", save_project, envir = network)
  assign("add_state", add_state, envir = network)
  class(network) <- c("environment", "xafty_network")

  if(!is.null(projects)) {
    for (project in projects) {
      network$add_project(project)
    }
  }
  network
}

create_add_project <- function(network) {
  force(network)
  add_project <- function(name, info = NULL, ...) {
    validate_project_name(name = name, network = network)
    network <- add_new_project(project = name, network = network)
    network <- set_project_print_order(projects = name, network = network)
    network <- set_project_info(project = name, info = info, network = network)
    invisible(network)
  }
  add_project
}

add_new_project <- function(project, network) {
  project_env <- new.env()
  class(project_env) <- c("environment", "xafty_project")
  network[[project]] <- project_env
  network[[project]][["ruleset"]] <- list(
    nodes = list(
      variables = list(), links = list(), joins = list()
    ),
    contexts = list(),
    groups = list(),
    settings = settings(name = project)
  )
  network[[project]][["add_group"]] <- create_add_group(project = project, network = network)
  network[[project]][["add_context"]] <- create_add_context(project = project, network = network)
  network[[project]][["link"]] <- compose_link_function(project = project, network = network, func_type = "link")
  invisible(network)
}

create_save_project <- function(network) {
  save_project <- function(file_name, path) {
    full_path <- paste0(path, "/", file_name, ".rds")
    save(network, file = full_path)
  }
  save_project
}

create_add_group <- function(project, network) {
  add_group <- function(name, vars = NULL) {
    group_container <- list(
      variables = vars
    )
    network[[project]][["ruleset"]][["groups"]][[name]] <- group_container
  }
  add_group
}

create_add_context <- function(project, network) {
  force(project)
  force(network)
  add_context <- function(name, on_entry = NULL, on_exit = NULL, overwrite = FALSE, ...) {
    enquo_on_exit <- rlang::enquo(on_exit)
    on_exit <- rlang::quo_get_expr(enquo_on_exit)
    enquo_on_entry <- rlang::enquo(on_entry)
    on_entry <- rlang::quo_get_expr(enquo_on_entry)
    if(is.null(on_entry) & is.null(on_exit)) stop("Please provide an on_entry or an on_exit function")
    if(!is.null(on_entry)) {
      register(quosure = enquo_on_entry, link_type = "context", func_type = "entry", network = network,
               project = project, name = name,  overwrite = overwrite, ...)
    }
    if(!is.null(on_exit)) {
      register(quosure = enquo_on_exit, link_type = "context", func_type = "exit", network = network,
               project = project, name = name, overwrite = overwrite, ...)
    }
  }
  add_context
}

compose_link_function <- function(project, network, func_type) {
  force(project)
  force(network)
  link <- function(fun, name = NULL, vars = NULL, attach_context = NULL, group = NULL, update = FALSE, direction = "one", ...) {
    .dots <- list(...)
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, link_type = "query", network = network, project = project,
             vars = vars, name = name, update = update, func_type = func_type, direction = direction,
             group = group, attach_context = attach_context, ... = .dots)
  }
  link
}

#' Merge networks into one
#' @description
#' The function allows the merging of any numbers of networks into one network. The return value is therefore always one network.
#' @param name Character vector of length 1. Name of the merged network
#' @param ... Networks that should be merged
#' @returns A 'xafty_network' environment
#' @export
merge_networks <- function(name, ...) {
  passed_networks <- list(...)
  li_projects <- lapply(passed_networks, \(network_env) {
    network_names <- names(network_env)
    network_names[vapply(network_names, \(nn) is.environment(network_env[[nn]]), FUN.VALUE = logical(1))]
  })

  projects <- unlist(li_projects)
  new_network_env <- init_network(name = name, projects = projects)
  # TODO: merge network must also merge print order of all merged projects
  # TODO: States with the same name from different networks need to be handled here
  li_states <- lapply(passed_networks, \(network_env) network_env$states)
  li_states <- do.call(append, li_states)
  new_network_env$states <- li_states

  index <- seq_along(passed_networks)
  for (i in index) {
    projects <-  li_projects[[i]]
    network <- passed_networks[[i]]
    for (project in projects) {
      ruleset <- network[[project]]$ruleset
      new_network_env[[project]]$ruleset <- ruleset
    }
  }
  class(new_network_env) <- c("xafty_network", "environment")
  new_network_env
}

validate_project_name <- function(name, network) {
  if(length(name) == 0 | length(name) > 1) stop("Please enter the project's name.")
  if(!identical(name, make.names(name))) stop("Please enter a valid project name")
  reserved_names <- c("save", "add_project", "query", "settings")
  if(name %in% reserved_names) stop(paste0("Please don't use any of the following reserved names as project names: ", paste0(reserved_names, collapse = ", "), "."))
  names_network <- names(network)
  existing_projects <- names_network[vapply(names_network, \(project) is.environment(network[[project]]), FUN.VALUE = logical(1))]
  if(any(name %in% existing_projects)) stop(paste0("Project '", name, "' exists already in network"))
}

create_add_state <- function(network_env) {
  add_state <- function(name, allowed = NULL, example = NULL, default = NULL, documentation = NULL) {
    if(length(name) == 0 | length(name) > 1) stop("Please enter the state's name.")
    if(!is_curly_character(name)) {
      test_name <- paste0("{", name, "}")
    } else {
      test_name <- name
    }
    if(!is_state_variable(test_name)) stop("Please enter a valid state name")
    existing_states <- names(network_env$states)
    state_exists <- name %in% existing_states
    user_choice <- "y"
    if(state_exists){
      user_choice <- readline(paste0("State '", name, "' was already added to the network '",  network_env$settings$network_name,"'. Would you like to update? (y/n): "))
    }
    if(user_choice == "y" | user_choice == "Y") {
      network_env$states[[name]] <- list(
        allowed = allowed,
        example = example,
        default = default,
        docu = documentation
      )
    } else {
      warning(paste0("State ", name, " was not added to the network!"))
    }
  }
}
