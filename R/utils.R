#' Print a xafty Network
#' @description
#' S3 method to print a xafty network.
#' @export
print.xafty_network <- function(x, ...) {
  values_in_x <- names(x)
  logcal_env <- vapply(values_in_x, \(project) is.environment(x[[project]]), FUN.VALUE = logical(1))
  projects <- names(logcal_env)[logcal_env]
  cat(paste0("Number of Projects in Network: ", length(projects), "\n"))
  cat("\n")
  for(project in projects) {
    columns <- names(x[[project]]$variables)
    joins <- names(x[[project]]$joined_projects)
    columns_print <- ifelse(length(columns) > 0, paste0(columns, collapse = ", "), "'none'")
    joins_print <- ifelse(length(joins) > 0, paste0(joins, collapse = ", "), "'none'")
    cat(paste0("Project: ", project, "\n"))
    cat(paste0("Columns: ", columns_print, "\n"))
    cat(paste0("Joins: ", joins_print, "\n"))
    cat("\n")
  }
}

unpack <- function(quosure, link_type = NULL, network_env) {
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_arg_specifics <- unpack_args(exp = fun_exp, env = fun_env)
  package_name <- get_function_package(func_name = list_arg_specifics$fun_name)
  if(is.na(package_name)) package_name <- "unkown"
  list_args_evaluated  <- eval_args(list_arg_specifics$fun, list_arg_specifics$args, env = fun_env, network_env = network_env)
  c(list(
      fun_name = list_arg_specifics$fun_name,
      fun = list_arg_specifics$fun,
      args = list_arg_specifics$args,
      package = package_name,
      link_type = link_type), list_args_evaluated
    )
}

eval_args <- function(fun, args, env, network_env) {
  value_list <- list(
    first = NULL,
    second  = NULL
  )
  n_args <- length(args)
  if (n_args >= 1) {
    value_list$first <- args[[1]]
    if(inherits(value_list$first, "xafty_link_list")) {
      value_list$first <- nascent(network = network_env, xafty_list = value_list$first)
      args[[1]] <- value_list$first
    }
  }
  if (n_args >= 2) {
    value_list$second <- args[[2]]
    if(inherits(value_list$second, "xafty_link_list")) {
      value_list$second <- nascent(network = network_env, xafty_list = value_list$second)
      args[[2]] <- value_list$second
    }
  }
  value_list$output <- do.call(fun, args, envir = env)

  list_value_return <- lapply(value_list, \(value) {
    cols <- NULL
    rows <- NULL
    class <-  class(value)
    if("data.frame" %in% class) {
      rows <- nrow(value)
      cols <- colnames(value)
    }
    list(
      cols = cols,
      class = class,
      rows = rows
    )
  })
  list_value_return
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

handle_dots_args <- function(fun_args, matched_call, env) {
  call_expr <- as.list(matched_call[["..."]])
  n_dot_args <- length(call_expr)
  pos_dots <- which(names(fun_args) == "..." )
  pos_dots_args <- seq(pos_dots, (pos_dots + n_dot_args - 1))
  for (i in seq(n_dot_args)) {
    into_arg <- pos_dots_args[i]
    fun_args[[into_arg]] <- rlang::eval_tidy(call_expr[[i]], env = env)
  }
  names(fun_args)[pos_dots_args] <- names(call_expr)
  fun_args
}

validate_link_type <- function(link_type, unpacked) {
  if (!"data.frame" %in% unpacked$output$class) stop(paste0("Return value of function '", unpacked$fun_name, "' must be a data.frame"))
  if (link_type == "add") {
    if(!"data.frame" %in% unpacked$first$class) stop(paste0("When registering a function with link type 'add', the first argument of function '", specifics$fun_name,"' must be a data.frame."))
    if(!unpacked$output$rows == unpacked$first$rows) stop("When registering a function with link type 'add', both input and output data.frames must have the same number of observations (rows).")
    if(!all(unpacked$first$cols %in% unpacked$output$cols)) stop("All passed columns need to be present in the output")
  }

  if(link_type == "join") {
    if(!"data.frame" %in% unpacked$first$class) stop("When registering a function with link type 'join', the first argument must be a data.frame")
    if(!"data.frame" %in% unpacked$second$class) stop("When registering a function with link type 'join', the second argument must be a data.frame")
    if(!all(c(unpacked$first$cols, unpacked$second$cols) %in% unpacked$output$cols)) stop("All passed columns need to be present in the output")
  }
  invisible(TRUE)
}


helper_pull_project <- function(columns, xafty_link) {
  pull_project <- sapply(xafty_link, \(link) {
    which(columns %in% link$pull)
  }, simplify = FALSE, USE.NAMES = TRUE)
  tmp_container <- character(sum(vapply(pull_project, \(what) length(what), FUN.VALUE = numeric(1))))
  for (i in seq_along(pull_project)) {
    pos <- pull_project[[i]]
    project <- names(pull_project[i])
    tmp_container[pos] <- project
  }
  tmp_container
}

get_join_projects <- function(project, ...) {
  dots <- list(...)
  join_projs <- unique(do.call(c, lapply(dots, \(pull) names(pull))))
  join_projs <- join_projs[!join_projs %in% project]
  if(length(join_projs) == 0) return(NULL)
  join_projs
}

build_dependency_codes <- function(from, dependencies, projects, joins) {
  if (is.null(dependencies)) return(character(0))
  column_depends <- unique(paste0(projects, ".", dependencies))
  if(is.null(joins)) return(column_depends)
  join_depends <- paste0("join.", paste0(from, ".", joins))
  c(column_depends, join_depends)
}

get_join_w_proj <- function(link) {
  into <- link$into
  join_w_proj <- c(link$left, link$right)
  join_w_proj <- join_w_proj[!join_w_proj == into]
  join_w_proj
}
