#' Initialize a New xafty Network
#' @description
#' Use this function to initialize a new xafty network.
#' @returns A 'xafty_network' environment
#' @examples
#' # Initialize the project
#' new_network <- init_network()
#'
#' # Add a new project
#' new_network$new_project("project1")
#'
#' @export
init_network <- function() {
  network_env <- new.env() # This is the list where all projects will be merged together
  new_project <- function(project) {
    if(length(project) == 0 | length(project) > 1) stop("Please enter the project's name")
    if(!identical(project, make.names(project))) stop("Please enter a valid project name")
    reserved_names <- c("save", "new_project")
    if(project %in% reserved_names) stop(paste0("Please don't use any of the following reserved names: ", paste0(reserved_names, collapse = ", "), ". These names are reserved for methods."))
    names_network <- names(network_env)
    existing_projects <- names_network[vapply(names_network, \(project) is.environment(network_env[[project]]), FUN.VALUE = logical(1))]
    if(any(project %in% existing_projects)) stop(paste0("Project '", project, "' exists already in network"))
    new_ruleset <- ruleset()
    new_settings <- settings()
    add_new_project(project = project, ruleset = new_ruleset, settings = new_settings, network_env = network_env)
  }
  assign("new_project", new_project, envir = network_env)
  invisible(network_env)
}

add_new_project <- function(project, ruleset, settings, network_env) {
  env_names <- c("variables", "joined_projects") # These will be environments for frequent look-ups during the nascent process

  project_env <- new.env() # This is the environment, where all code will be organized
  network_env[[project]] <- project_env
  for (env_name in env_names) {
    assign(env_name, new.env(), envir = project_env)
  }

  link_funs <- bundle_link_functions(project = project, env = network_env)

  link_names <- c("get", "add", "join")

  for (lp in link_names) {
    assign(lp, link_funs[[lp]], envir = project_env)
  }

  class(network_env) <- c("xafty_network", "environment")
  invisible(network_env)
}

create_get <- function(project, env) {
  force(project)
  force(env)
  get <- function(fun) {
    quosure <- rlang::enquo(fun)
    register(fun = quosure, link_type = "get", module = "link", network_env = env, project = project)
  }
  get
}

create_add <- function(project, env) {
  force(project)
  force(env)
  add <- function(fun) {
    quosure <- rlang::enquo(fun)
    register(fun = quosure, link_type = "add", module = "link", network_env = env, project = project)
  }
  add
}

create_join <- function(project, env) {
  force(project)
  force(env)
  join <- function(with, fun) {
    quosure <- rlang::enquo(fun)
    register(fun = quosure, link_type = "join", module = "link", network_env = env, project = c(project, with))
  }
  join
}

bundle_link_functions <- function(project, env) {
  get_fun <- create_get(project = project, env = env)
  add_fun <- create_add(project = project, env = env)
  join_fun <- create_join(project = project, env = env)

  list("get" = get_fun,
       "add" = add_fun,
       "join" = join_fun)
}


merge_networks <- function(...) {
  new_network_env <- init_network()
  passed_networks <- list(...)
  li_projects <- lapply(passed_networks, \(project_env) {
    network_names <- names(project_env)
    network_names[vapply(network_names, \(nn) is.environment(project_env[[nn]]), FUN.VALUE = logical(1))]
  })

  index <- seq_along(passed_networks)
  lapply(index, \(i) {
    vec_projects <-  li_projects[[i]]
    if(length(vec_projects) <= 0) next
    network_env <- passed_networks[[i]]
    for (project in vec_projects) {
      link_funs <- bundle_link_functions(project = project, env = new_network_env)
      link_names <- c("get", "add", "join")
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
