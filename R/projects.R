#' Initialize a New xafty Network
#' @description
#' Use this function to initialize a new xafty network.
#' @param name Character vector of length 1. A valid name for the network
#' @param projects Character vector of project names that should be added to the network
#' @param containers Character vector of container names that should be added to the network
#' @returns A 'xafty_network' environment
#' @examples
#' # Initialize the network
#' new_network <- init_network(name = "network_1")
#'
#' # Add a new project
#' new_network$add_project("project1")
#'
#' @export
init_network <- function(name, projects = NULL, containers = NULL) {
  network_env <- new.env() # This is the list where all projects will be merged together
  network_env$settings <- settings(network_name = name)
  network_env$states <- list()
  add_project <- create_add_project(network_env = network_env)
  add_container <- create_add_container(network_env = network_env)
  save_project <- create_save_project(network_env = network_env)
  add_state <- create_add_state(network_env = network_env)
  assign("add_project", add_project, envir = network_env)
  assign("add_container", add_container, envir = network_env)
  assign("save", save_project, envir = network_env)
  assign("add_state", add_state, envir = network_env)
  class(network_env) <- c("environment", "xafty_network")

  if(!is.null(projects)) {
    for (project in projects) {
      network_env$add_project(project)
    }
  }
  if(!is.null(containers)) {
    for (container in containers) {
      network_env$add_container(container)
    }
  }
  network_env
}

create_add_project <- function(network_env) {
  force(network_env)
  add_project <- function(name, ...) {
    validate_project_name(name = name, network = network_env)
    new_ruleset <- ruleset()
    .network_env <- add_new_project(project = name, ruleset = new_ruleset, network_env = network_env,
                                    link_types = c("get", "add", "join", "add_object"))
    invisible(.network_env)
  }
  add_project
}

create_add_container <- function(network_env) {
  force(network_env)
  add_container <- function(name, ...)  {
    validate_project_name(name = name, network = network_env)
    project_config <- list(...)
    new_ruleset <- ruleset()
    .network_env <- add_new_project(project = name, ruleset = new_ruleset, network_env = network_env,
                                    link_types = c("add", "add_object"))
    project_env <- .network_env[[name]]
    class(project_env) <- c("xafty_container", "environment")
    invisible(.network_env)
  }
  add_container
}

create_save_project <- function(network_env) {
  save_project <- function(file_name, path) {
    full_path <- paste0(path, "/", file_name, ".rds")
    save(network_env, file = full_path)
  }
  save_project
}

add_new_project <- function(project, ruleset, network_env, link_types = c("get", "add", "join", "add_object")) {
  env_names <- c("variables", "joined_projects", "objects") # These will be environments for frequent look-ups during the nascent process

  project_env <- new.env() # This is the environment, where all code will be organized
  class(project_env) <- c("xafty_project", "environment")
  network_env[[project]] <- project_env
  for (env_name in env_names) {
    assign(env_name, new.env(), envir = project_env)
  }

  link_funs <- bundle_link_functions(project = project, env = network_env)

  for (lp in link_types) {
    assign(lp, link_funs[[lp]], envir = project_env)
  }
  invisible(network_env)
}

create_add_object <- function(project, env) {
  force(project)
  force(env)
  add_object <- function(name, fun, ...) {
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, module = "object", network = env, project = project, object_name = name, ...)
  }
  add_object
}

create_get <- function(project, env) {
  force(project)
  force(env)
  get <- function(fun, ...) {
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, module = "link", network = env, project = project, ... = ...)
  }
  get
}

create_add <- function(project, env) {
  force(project)
  force(env)
  add <- function(fun, ...) {
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, module = "link", network = env, project = project, ... = ...)
  }
  add
}

create_join <- function(project, env) {
  force(project)
  force(env)
  join <- function(fun, ...) {
    quosure <- rlang::enquo(fun)
    register(quosure = quosure, module = "link", network = env, project = project, ... = ...)
  }
  join
}

bundle_link_functions <- function(project, env) {
  get_fun <- create_get(project = project, env = env)
  add_fun <- create_add(project = project, env = env)
  join_fun <- create_join(project = project, env = env)
  add_object <- create_add_object(project = project, env = env)
  list("get" = get_fun,
       "add" = add_fun,
       "join" = join_fun,
       "add_object" = add_object)
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

  # TODO: States with the same name need to be handled here
  li_states <- lapply(passed_networks, \(network_env) network_env$states)
  li_states <- do.call(append, li_states)
  new_network_env$states <- li_states

  index <- seq_along(passed_networks)
  lapply(index, \(i) {
    vec_projects <-  li_projects[[i]]
    if(length(vec_projects) <= 0) return(NULL)
    network_env <- passed_networks[[i]]
    for (project in vec_projects) {
      link_funs <- bundle_link_functions(project = project, env = new_network_env)
      link_names <- c("get", "add", "join", "add_object")
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
    existing_states <- names(network_env$sates)
    state_exists <- name %in% existing_states
    # TODO Ask user if they want to overwrite the state
    network_env$states[[name]] <- list(
      allowed = allowed,
      example = example,
      default = default,
      docu = documentation
    )
  }
}
