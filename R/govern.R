govern <- function(network = NULL) {

  state_env <- new.env()

  ##################################
  ######## Global  State ###########
  ##################################

  set_data_key <- function(project, key) {
    state_env$projects[[project]]$key <- key
  }

  get_data_key <- function(project) {
    state_env$projects[[project]]$key
  }

  set_data <- function(data, key) {
    state_env$data[[key]] <- data
  }

  get_data <- function(project) {
    key <- get_data_key(project)
    state_env$data[[key]]
  }

  get_data_by_key <- function(key) {
    state_env$data[[key]]
  }

  set_dependencies <- function(project, cols) {
    new_dependencies <- c(get_dependencies(project), cols)
    non_already_pulled <- new_dependencies[!new_dependencies %in% get_pulls(project)]
    if(length(non_already_pulled) > 0) {
      state_env$projects[[project]]$dependencies <- non_already_pulled
    }
  }

  set_fun_pair <- function(project, fun_name, code) {
    list_pairing <- list(
      project = project,
      fun = fun_name
    )
    state_env$fun_pairs[[code]] <- list_pairing
  }

  get_fun_pair <- function(code) {
    state_env$fun_pairs[[code]]
  }

  set_function_stack <- function(project, fun_name, fun_code, deps, push) {
    current_stack <- get_function_stack(project = project)
    codes <- build_dependency_codes(deps)
    set_fun_pair(project = project, fun_name = fun_name, code = fun_code)
    set_pulls(project, push)
    add_to_stack <- setNames(list(codes), fun_code)
    if(!fun_code %in% names(current_stack)) {
      new_stack <- c(current_stack, add_to_stack)
      state_env$projects[[project]]$fun_stack <- new_stack
    }
  }

  set_pulls <- function(project, pull) {
    current_pull <- get_pulls(project)
    new_pulls <- unique(pull[!pull %in% current_pull])
    if(length(new_pulls) > 0) {
      new_pull_stack <- c(current_pull, new_pulls)
      state_env$projects[[project]]$pulls <- new_pull_stack
      remove_dependencies(project, pull)
    }
  }

  set_join_list <- function(li) {
    state_env$join_list <- li
  }

  remove_dependencies <- function(project, cols) {
    dependencies <- get_dependencies(project)
    dependencies <- dependencies[!dependencies %in% cols]
    if(length(dependencies) <= 0) {
      dependencies <- NULL
    }
    state_env$projects[[project]]$dependencies <- dependencies
  }

  get_dependencies <- function(project) {
    state_env$projects[[project]]$dependencies
  }

  get_join_list <- function() {
    state_env$join_list
  }


  get_pulls <- function(project) {
    state_env$projects[[project]]$pulls
  }


  get_function_stack <- function(project) {
    state_env$projects[[project]]$fun_stack
  }

  get_projects <- function() {
    names(state_env$projects)
  }

  set_join_path <- function(path) {
    state_env$join_path <- path
  }

  get_join_path <- function() {
    join_path <-  state_env$join_path
    if(is.null(join_path)) return(list())
    join_path
  }
  # Sets up a table environments for each Project

  execute_stack <- function(fun) {
    arg_names <- fun$network$arg_defs$names
    projects <- get_all_projects(fun)
    if (!length(arg_names) == 0) { # set correct args
      fun$ruleset$args <-   sapply(arg_names, \(name) build_executable_args(name = name, fun = fun, projects = projects, get_data = get_data), simplify = FALSE, USE.NAMES = TRUE)
    }
    # Doing this to avoid too much memory use, is this necessary?
    for (project in projects) {
      if(!is.null(get_data_key(project))) {
        set_data(data = NULL, key = get_data_key(project))
      }
    }
    new_key <- paste0(projects, collapse = "_")
    data <- do.call(fun$ruleset$fun, fun$ruleset$args)
    projects_update_key <- do.call(c, lapply(projects, \(project) {
      key <- get_data_key(project)
      if(is.null(key)) return(project)
      get_projects_by_key(key)
      }))
    for(proj in projects_update_key) {
      set_data_key(project = proj, key = new_key)
    }
    set_data(data = data, key = new_key)
  }

  get_projects_by_key <- function(key) {
    projects <- get_projects()
    contains_key <- vapply(projects, \(project) {
      key_value <- state_env$projects[[project]]$key
      if(is.null(key_value)) return(FALSE)
      key_value == key
    } , FUN.VALUE = logical(1))
    names(contains_key)[contains_key]
  }

  list(
    # Global state
    get_projects = get_projects,
    set_data_key = set_data_key,
    get_data_key = get_data_key,
    set_data = set_data,
    get_data = get_data,
    set_pulls = set_pulls,
    get_pulls = get_pulls,
    set_join_list = set_join_list,
    get_join_list = get_join_list,
    set_dependencies = set_dependencies,
    get_dependencies = get_dependencies,
    set_function_stack = set_function_stack,
    get_function_stack = get_function_stack,
    set_join_path = set_join_path,
    get_join_path = get_join_path,
    execute_stack = execute_stack,
    set_fun_pair = set_fun_pair,
    get_fun_pair = get_fun_pair,
    get_projects_by_key = get_projects_by_key,
    get_data_by_key = get_data_by_key
  )

}
