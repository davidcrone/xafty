register <- function(fun, link_type = c("get", "add", "join"), module = "link", network_env = NULL, project = NULL) {
  stopifnot(!is.null(project))
  stopifnot(!is.null(link_type))
  link_type <- link_type[[1]]
  if(link_type == "join" & length(project) <= 1) stop("When linking with link type join, a character vector of length two must be supplied containing the name of both joined projects")
  quosure <- fun
  unpacked <- unpack(quosure, link_type = link_type, network_env = network_env)

  if (link_type == "get") {
    register_get(project = project, unpacked = unpacked, env = network_env)
  } else if (link_type == "add") {
    register_add(project = project, unpacked = unpacked, env = network_env)
  } else if (link_type == "join") {
    register_join(project = project, unpacked = unpacked, env = network_env)
  } else {
    stop(paste0("link type ", link_type), " not recognized")
  }
}

register_join <- function(project, unpacked, env, evaluate = TRUE) {
  if(evaluate) {
    validate_link_type(link_type = "join", unpacked = unpacked)
  }
  specs <- build_join_links(unpacked = unpacked, project = project, network = env)
  env <- add_to_ruleset(specs, module = "link", env = env, project = project)
  env <- add_to_network_joined_projects(specs, env = env, into = project[1], join = project[2])
  env <- add_to_network_joined_projects(specs, env = env, into = project[2], join = project[1])
  invisible(env)
}

register_add <- function(project, unpacked, env, evaluate = TRUE) {
  if(evaluate) {
    validate_link_type(link_type = "add", unpacked = unpacked)
  }
  specs <- build_add_links(unpacked = unpacked, project = project, network = env)
  env <- add_to_ruleset(specs, module = "link", env = env, project = project)
  env <- add_to_network_variables(specs, env = env, project = project)
  invisible(env)
}

register_get <- function(project, unpacked, env, evaluate = TRUE) {
  if(evaluate) {
    validate_link_type(link_type = "get", unpacked = unpacked)
  }
  if(length(project) > 1) stop("When linking with link type 'get', only a single project may be registered.")
  specs <- build_get_links(unpacked = unpacked, project = project)
  env <- add_to_ruleset(specs, module = "link", env = env, project = project)
  env <- add_to_network_variables(specs, env = env, project = project)
  invisible(env)
}

build_get_links <- function(unpacked, project) {
  network_specs <- list(
    list(
      call = list(
        fun = unpacked$fun,
        args = unpacked$args
      ),
      info = list(
        link_type = unpacked$link_type,
        package = unpacked$package,
        project = project
      ),
      network = list(
        fun_name = unpacked$fun_name,
        pull = NULL,
        from = NULL,
        exec = NULL,
        push = unpacked$output$cols,
        into = project,
        joins = NULL
      )
    )
  )
  network_specs <- setNames(network_specs, unpacked$fun_name)
  network_specs
}


build_add_links <- function(unpacked, project, network) {
  first_arg <- unpacked$args[[1]]
  pull <- unpacked$first$cols
  if(inherits(first_arg, "data.frame")) {
    first_arg <- do.call(pull_link, setNames(list(pull), project))
  }
  pull_project <- helper_pull_project(pull, first_arg)
  push <- unpacked$output$cols[!(unpacked$output$cols %in% pull)]
  push_project <- rep(project, length(push))
  exec <- mapply(parse_network_values, pull, pull_project, MoreArgs = list(network = network, what = "fun_name"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  exec <- do.call(c, exec)
  joins <- get_join_projects(project = project, first_arg)
  unpacked$args[[1]] <- first_arg
  network_specs <- list(
    list(
      call = list(
        fun = unpacked$fun,
        args = unpacked$args
      ),
      info = list(
        fun_name = unpacked$fun_name,
        link_type = unpacked$link_type,
        package = unpacked$package,
        project = project
      ),
      network = list(
        fun_name = unpacked$fun_name,
        pull = pull,
        from = pull_project,
        exec = exec,
        push = push,
        into = push_project,
        joins = joins
      )
    )
  )
  network_specs <- setNames(network_specs, unpacked$fun_name)
  network_specs
}

parse_network_values <- function(pull, from, network, what) {
  network[[from]]$variables[[pull]][[what]]
}


build_join_links <- function(unpacked, project, network) {
  first_arg <- unpacked$args[[1]]
  second_arg <- unpacked$args[[2]]
  pull_first <- unpacked$first$cols
  pull_second <- unpacked$second$cols
  if(inherits(first_arg, "data.frame")) {
    first_arg <- do.call(pull_link, setNames(list(pull_first), project[1]))
  }
  if(inherits(second_arg, "data.frame")) {
    second_arg <- do.call(pull_link, setNames(list(pull_second), project[2]))
  }
  pull <- c(pull_first, pull_second)
  pull_project_first <- helper_pull_project(pull_first, first_arg)
  pull_project_second <- helper_pull_project(pull_second, second_arg)
  pull_project <- c(pull_project_first, pull_project_second)
  push <- NULL
  push_project <- project[1]
  exec <- mapply(parse_network_values, pull, pull_project, MoreArgs = list(network = network, what = "fun_name"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  exec <- do.call(c, exec)
  joins <- get_join_projects(project = project, first_arg, second_arg)
  unpacked$args[[1]] <- first_arg
  unpacked$args[[2]] <- second_arg
  network_specs <- list(
    list(
      call = list(
        fun = unpacked$fun,
        args = unpacked$args
      ),
      info = list(
        fun_name = unpacked$fun_name,
        link_type = unpacked$link_type,
        package = unpacked$package,
        project = project
      ),
      network = list(
        fun_name = unpacked$fun_name,
        pull = pull,
        from = pull_project,
        exec = exec,
        push = push,
        into = push_project,
        left = project[1],
        right = project[2],
        joins = joins
      )
    )
  )
  network_specs <- setNames(network_specs, unpacked$fun_name)
  network_specs
}

#' Get the Package Name of a Function
#'
#' This function determines the package where a given function is defined.
#'
#' @param func_name A character string specifying the name of the function.
#' @return A character vector of package names where the function is found, or `NA` if the function is not found in any package.
#' @examples
#' get_function_package("mean")   # Returns "base"
#' get_function_package("filter") # Returns "dplyr" (if dplyr is loaded)
#' get_function_package("non_existent_function") # Returns NA
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

