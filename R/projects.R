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
  network_env <- new.env() # This is the network where all projects will be merged together
  network_env$settings <- settings(name = name)
  network_env$states <- list()
  add_project <- create_add_project(network_env = network_env)
  save_project <- create_save_project(network_env = network_env)
  add_state <- create_add_state(network_env = network_env)
  assign("add_project", add_project, envir = network_env)
  assign("save", save_project, envir = network_env)
  assign("add_state", add_state, envir = network_env)
  class(network_env) <- c("environment", "xafty_network")

  if(!is.null(projects)) {
    for (project in projects) {
      network_env$add_project(project)
    }
  }
  network_env
}

create_add_project <- function(network_env) {
  force(network_env)
  add_project <- function(name, info = NULL, ...) {
    validate_project_name(name = name, network = network_env)
    .network_env <- add_new_project(project = name, network_env = network_env,
                                    func_types = c("link", "on_entry", "on_exit"))
    .network_env <- set_project_print_order(projects = name, network = .network_env)
    .network_env <- set_project_info(project = name, info = info, network = network_env)
    invisible(.network_env)
  }
  add_project
}

create_save_project <- function(network_env) {
  save_project <- function(file_name, path) {
    full_path <- paste0(path, "/", file_name, ".rds")
    save(network_env, file = full_path)
  }
  save_project
}

add_new_project <- function(project, network_env, func_types) {
  env_names <- c("variables", "joined_projects", "groups") # These will be environments for frequent look-ups during the nascent process
  project_env <- new.env() # This is the environment, where all code will be organized
  class(project_env) <- c("environment", "xafty_project")
  network_env[[project]] <- project_env
  network_env[[project]][["ruleset"]] <- list()
  network_env[[project]][["settings"]] <- settings(name = project)
  network_env[[project]][["add_group"]] <- create_add_group(project = project, network = network_env)
  for (env_name in env_names) {
    assign(env_name, new.env(), envir = project_env)
  }
  link_funs <- bundle_link_functions(project = project, network = network_env)
  for (lp in func_types) {
    assign(lp, link_funs[[lp]], envir = project_env)
  }
  invisible(network_env)
}

create_add_group <- function(project, network) {
  add_group <- function(name, vars = NULL, on_entry = NULL, on_exit = NULL) {
    group_container <- list(
      variables = vars,
      contexts = list(
        on_entry = on_entry,
        on_exit = on_exit
      )
    )
    network[[project]][["groups"]][[name]] <- group_container
  }
  add_group
}

create_add_context <- function(project, network, func_type = NULL) {
  force(project)
  force(network)
  force(func_type)
  add_context <- function(fun, name = NULL, group, update = FALSE, ...) {
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, link_type = "context", network = network,
             project = project, name = name, update = update, func_type = func_type, group = group, ...)
  }
  add_context
}

compose_link_function <- function(project, network, func_type) {
  force(project)
  force(network)
  link <- function(fun, name = NULL, vars = NULL, group = NULL, update = FALSE, direction = "one", ...) {
    .dots <- list(...)
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, link_type = "query", network = network, project = project,
             vars = vars, name = name, update = update, func_type = func_type, direction = direction, group = group, ... = .dots)
  }
  link
}

bundle_link_functions <- function(project, network) {
  link_fun <- compose_link_function(project = project, network = network, func_type = "link")
  on_entry_fun <- create_add_context(project = project, network = network, func_type = "entry")
  on_exit_fun <- create_add_context(project = project, network = network, func_type = "exit")
  list(
    "on_entry" = on_entry_fun,
    "link" = link_fun,
    "on_exit" = on_exit_fun
  )
}


#' Merge networks into one
#' @description
#' The function allows the merging of any numbers of networks into one network. The return value is therefore always one network.
#' @param name Character vector of length 1. Name of the merged network
#' @param ... Networks that should be merged
#' @returns A 'xafty_network' environment
#' @export
merge_networks <- function(name, ...) {
  new_network_env <- init_network(name = name)
  passed_networks <- list(...)
  li_projects <- lapply(passed_networks, \(network_env) {
    network_names <- names(network_env)
    network_names[vapply(network_names, \(nn) is.environment(network_env[[nn]]), FUN.VALUE = logical(1))]
  })
  # TODO: merge network must also merge print order of all merged projects
  # TODO: States with the same name from different networks need to be handled here
  li_states <- lapply(passed_networks, \(network_env) network_env$states)
  li_states <- do.call(append, li_states)
  new_network_env$states <- li_states

  index <- seq_along(passed_networks)
  lapply(index, \(i) {
    vec_projects <-  li_projects[[i]]
    if(length(vec_projects) <= 0) return(NULL)
    network_env <- passed_networks[[i]]
    for (project in vec_projects) {
      link_funs <- bundle_link_functions(project = project, network = new_network_env)
      link_names <- "link"
      project_env <- network_env[[project]]
      rm(list = link_names, envir = project_env)
      for (lp in link_names) {
        assign(lp, link_funs[[lp]], envir = project_env)
      }
      assign(project, project_env, envir = new_network_env)
    }
    invisible(TRUE)
  })

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
