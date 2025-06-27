#' Print a xafty Network
#' @description
#' S3 method to print a xafty network.
#' @export
print.xafty_network <- function(x, ...) {
  values_in_x <- names(x)
  logcal_env <- vapply(values_in_x, \(project) is.environment(x[[project]]), FUN.VALUE = logical(1))
  projects <- names(logcal_env)[logcal_env]
  cat(paste0("Number of projects in network: ", length(projects), "\n"))
  cat("\n")
  for(project in projects) {
    is_sub <- "xafty_bundle" %in% class(x[[project]])
    columns <- names(x[[project]]$variables)
    joins <- names(x[[project]]$joined_projects)
    columns_print <- ifelse(length(columns) > 0, paste0(columns, collapse = ", "), "'none'")
    joins_print <- ifelse(length(joins) > 0, paste0(joins, collapse = ", "), "'none'")
    if (is_sub) {
      cat(paste0("Sub Project: ", project, "\n"))
    } else {
      cat(paste0("Project: ", project, "\n"))
    }
    cat(paste0("Columns: ", columns_print, "\n"))
    cat(paste0("Joins: ", joins_print, "\n"))
    cat("\n")
  }
}
# TODO:
## Must: Within Query: Are projects joined?
## Needed?: Across Queries: Are projects not duplicated? Is this necessary?

unpack <- function(quosure, network, project) {
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_network <- args_analysis(fun = list_args$fun, args = list_args$args, fun_env = fun_env, network = network)
  list_info <- info_list(list_args = list_args, project = project)
  #
  list(
    ruleset = list_args,
    network = list_network,
    info = list_info
  )
}

args_analysis <- function(fun, args, fun_env, network) {
  links <- vapply(args, \(arg) find_xafty_objects(arg), FUN.VALUE = character(1))
  arg_names <- names(links)
  names(links) <- NULL
  arg_defs <- list(
    names = arg_names,
    links = links
  )
  args_evals <- mapply(exe_query, args, links, SIMPLIFY = FALSE, USE.NAMES = TRUE, MoreArgs = list(network = network))
  li_arg_dependencies <- gather_dependencies_per_arg(args = args, defs = arg_defs, network = network)
  li_output <- gather_output_info(fun = fun, args = args_evals, arg_deps = li_arg_dependencies, defs = arg_defs, fun_env = fun_env)
  list(
    dependencies = li_arg_dependencies,
    output = li_output,
    arg_defs = arg_defs
  )
}

info_list <- function(list_args, project) {
  package_name <- get_function_package(func_name = list_args$fun_name)
  list(
    package = package_name,
    project = project
  )
}

gather_output_info <- function(fun, args, arg_deps, defs, fun_env) {
  arg_names <- defs$names
  input_cols <- do.call(c, lapply(arg_names, \(name) {
    def <- defs$links[defs$names == name]
    if(def == "xafty_query") {
      return(colnames(args[[name]]))
    }
    character(0)
  }))
  vec_lead_project <- do.call(c, lapply(arg_names, \(name) {
    def <- defs$links[defs$names == name]
    if(def == "xafty_query") {
      return(arg_deps[[name]]$lead)
    }
    character(0)
  }))
  output_fun <- do.call(fun, args = args, envir = fun_env)
  if(!class(output_fun) == "data.frame") stop("Return value of function '", output_fun, "' must inherit data.frame")
  output_cols <- colnames(output_fun)
  if(!all(input_cols %in% output_cols)) {
    cols_missing <- input_cols[!input_cols %in% output_cols]
    cols_missing_text <- paste0(cols_missing, collapse = ", ")
    stop(paste0("Not all input columns present in output. Following columns are missing: ", cols_missing_text))
  }
  added_cols <- output_cols[!output_cols %in% input_cols]
  li_join_pairs <- get_ordered_join_pairs(vec_lead_project)
  added_joins <- build_join_pairs(li_join_pairs)
  list(
    added_cols = added_cols,
    added_joins = added_joins,
    join_pairs = li_join_pairs
  )
}

gather_dependencies_per_arg <- function(args, defs, network) {
  arg_names <- defs$names
  deps <- sapply(arg_names, \(name) {
    def <- defs$links[defs$names == name]
    if (def == "xafty_query") {
      xafty_query <- args[[name]]
      projects <- names(xafty_query)
      lead_project <- projects[1]
      cols_deps <- sapply(xafty_query, \(q) {
        select <- q$select
        project <- q$from
        funs <- unique(vapply(select, \(s) network[[project]]$variables[[s]], FUN.VALUE = character(1)))
        names(funs) <- NULL
        list(
          select = select,
          funs = funs
        )
        }, simplify = FALSE, USE.NAMES = TRUE)
      join_deps <- projects[projects != lead_project]
      list(
        lead = lead_project,
        cols = cols_deps,
        joins = join_deps
      )
    } else if (def == "xafty_state") {
      # TODO: Resolve dependencies of xafty state
      NULL
    }
    else {
      NULL
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
  deps <- deps[!vapply(deps, \(dep) is.null(dep), FUN.VALUE = logical(1))]

  deps
}

exe_query <- function(arg, def, network) {
  if(def == "xafty_query") {
    nascent(network, arg)
  } else {
    arg
  }
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

build_dependency_codes <- function(deps) {
  if (length(deps) == 0) return(character(0))
  column_depends <- do.call(c, lapply(seq_along(deps), \(i) {
    arg <- deps[[i]]$cols
    dep_projects <- names(arg)
    do.call(c, lapply(dep_projects, \(proj) paste0(proj, ".", arg[[proj]]$funs)))
  }))
  join_depends <- do.call(c, lapply(deps, \(dep) {
    lead_project <- dep$lead
    join_projects <- dep$joins
    if(length(join_projects) > 0) {
      return(paste0("join.", lead_project, ".", join_projects))
    }
    character(0)
  }))
  c(column_depends, join_depends)
}

find_xafty_objects <- function(arg) {
  if("xafty_query_list" %in% class(arg)) return("xafty_query")
  if(is_xafty_state_variable(arg)) return("xafty_state")
  "none_xafty_object"
}

is_braced_variable <- function(arg) {
  grepl("^\\{[^{}]+\\}$", arg)
}

get_braced_variable <- function(arg) {
  match <- regmatches(arg, regexec("^\\{([^{}]+)\\}$", arg))[[1]]
  match[2]
}

is_valid_variable_name <- function(arg) {
  match <- get_braced_variable(arg)
  is_valid_variable_name <- identical(match, make.names(match))
  if(!is_valid_variable_name) stop(paste0("State variable detected, but '", match, "' is not a valid variable name"))
  is_valid_variable_name
}

is_xafty_state_variable <- function(arg) {
  if(!is.character(arg) || length(arg) != 1) return(FALSE)
  if(!is_braced_variable(arg)) return(FALSE)
  is_valid_variable_name(arg)
}

get_ordered_join_pairs <- function(projects) {
  n <- length(projects)
  pairs <- list()
  k <- 1
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        pairs[[k]] <- c(projects[i], projects[j])
        k <- k + 1
      }
    }
  }
  pairs
}

build_join_pairs <- function(li_pairs) {
  do.call(c, lapply(li_pairs, \(pair) {
    paste0("join.", pair[1], ".", pair[2])
  }))
}

build_executable_args <- function(name, fun, projects, get_data) {
  link <- fun$network$arg_defs$links[fun$network$arg_defs$names == name]
  if (link == "xafty_query") {
    lead_project <- fun$network$dependencies[[name]]$lead
    get_data(project = lead_project)
  } else if (link == "xafty_state") {
    # TODO get xafty state
    fun$ruleset$args[[name]]
  } else {
    fun$ruleset$args[[name]]
  }
}

get_all_projects <- function(item) {
  main_project <- item$info$project
  args_with_lead <- item$network$arg_defs$names["xafty_query" == item$network$arg_defs$link]
  lead_projects <- vapply(args_with_lead, \(arg) item$network$dependencies[[arg]]$lead, FUN.VALUE = character(1))
  unique(c(main_project, lead_projects))
}
