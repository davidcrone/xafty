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
    is_container <- "xafty_container" %in% class(x[[project]])
    columns <- names(x[[project]]$variables)
    joins <- names(x[[project]]$joined_projects)
    columns_print <- ifelse(length(columns) > 0, paste0(columns, collapse = ", "), "'none'")
    joins_print <- ifelse(length(joins) > 0, paste0(joins, collapse = ", "), "'none'")
    if (is_sub) {
      cat(paste0("Sub Project: ", project, "\n"))
    } else if (is_container) {
      cat(paste0("Container: ", project, "\n"))
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
  arg_defs <- build_arg_defs(links)
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

build_arg_defs <- function(links) {
  arg_names <- names(links)
  names(links) <- NULL
  arg_defs <- list(
    names = arg_names,
    links = links
  )
  arg_defs
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
      if (length(projects) <= 1) {
        join_deps <- character(0)
      } else {
        log_xafty_projects <- vapply(projects, \(project) "xafty_project" %in% class(network[[project]]), FUN.VALUE = logical(1))
        join_deps <- projects[log_xafty_projects]
      }
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

evaluate_arg <- function(arg, xo, network) {
  if(xo == "xafty_query") {
    nascent(network, arg)
  } else {
    arg
  }
}

eval_args <- function(link, network) {
  xo <- get_xafty_objects_vec(link)
  mapply(evaluate_arg, link$args, xo, MoreArgs = list(network = network), SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

execute_function <- function(link, network) {
  do.call(link$fun, eval_args(link, network))
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

build_dependency_codes <- function(link, network, sm) {
  queries <- get_queries(link)
  fun_code <- paste0(link$project, ".", link$fun_name)
  if (length(queries) == 0) {
    root_node <- setNames(list(character(0)), fun_code)
    return(root_node)
  }
  scoped_functions <- unique(do.call(c, lapply(link$args, get_scoped_function_order, network = network)))
  li_within_joins <- lapply(queries, get_joins_within_query, network = network)
  for (i in seq_along(li_within_joins)) {
    joins <- li_within_joins[[i]]
    if(length(joins) <= 0) next
    unordered_pairs <- combn(joins, 2, simplify = FALSE)
    join_codes <- build_join_pairs(unordered_pairs)
    li_within_joins[[i]] <- join_codes
    li_pairs <- setNames(unordered_pairs, join_codes)
    for (i in seq_along(li_pairs)) {
      sm$set_join_pairs(li_pairs[i])
    }
  }
  join_depends <- do.call(c, li_within_joins)

  node <- setNames(list(c(scoped_functions, join_depends)), fun_code)
  node
}

find_xafty_objects <- function(arg) {
  if("xafty_query_list" %in% class(arg)) return("xafty_query")
  if(is_xafty_state_variable(arg)) return("xafty_state")
  "none_xafty_object"
}

get_xafty_objects_vec <- function(link) {
  args <- link$args
  xafty_objects_vec <- vapply(args, find_xafty_objects, FUN.VALUE = character(1))
  xafty_objects_vec
}

get_queries <- function(link) {
  xafty_objects_vec <- get_xafty_objects_vec(link)
  arg_names_w_query <- names(xafty_objects_vec)[xafty_objects_vec == "xafty_query"]
  if(length(arg_names_w_query) <= 0) return(list())
  arg_w_query <- sapply(arg_names_w_query, \(arg_name) link$args[[arg_name]], simplify = FALSE, USE.NAMES = TRUE)
  arg_w_query
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

get_ordered_join_pairs <- function(link) {
  projects <- get_lead_projects(link)
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

build_executable_args <- function(link, get_data, mask) {
  func_args <- link$args
  lead_projects <- get_lead_projects(link)
  queries <- get_queries(link)
  args <- names(queries)
  for (i in seq_along(queries)) {
    project <- lead_projects[i]
    arg <- args[i]
    data <- get_data(project = project)
    data <- unscope(data = data, link = link, arg_name = arg, mask = mask)
    func_args[[arg]] <- data
  }
  func_args
}

get_all_projects <- function(item) {
  main_project <- item$info$project
  args_with_lead <- item$network$arg_defs$names["xafty_query" == item$network$arg_defs$link]
  lead_projects <- vapply(args_with_lead, \(arg) item$network$dependencies[[arg]]$lead, FUN.VALUE = character(1))
  unique(c(main_project, lead_projects))
}

get_lead_projects <- function(link) {
  queries <- get_queries(link)
  arg_w_query <- names(queries)
  # The lead project will always be the first project in a query
  vapply(arg_w_query, \(arg_name) queries[[arg_name]][[1]]$from, FUN.VALUE = character(1))
}

build_cartesian_product <- function(query) {
  df_cartesian <- do.call(rbind, lapply(query, \(sq) data.frame(project = rep(sq$from, length(sq$select)), column = sq$select)))
  row.names(df_cartesian) <- NULL
  df_cartesian
}

get_flattened_cartesian <- function(link) {
  queries <- get_queries(link)
  sapply(queries, build_cartesian_product, simplify = FALSE, USE.NAMES = TRUE)
}

get_dependend_functions <- function(link, network, scope = FALSE) {
  li_cartesian <- get_flattened_cartesian(link)
  sapply(li_cartesian, \(df) {
    n_row <- nrow(df)
    func_names <- vapply(seq(n_row), \(n) get_chatty_func_name_from_network(col = df$column[n], project = df$project[n], network = network),
                         FUN.VALUE = character(1))
    if(scope) return(paste0(df$project, ".", func_names))
    func_names
  }, simplify = FALSE, USE.NAMES = TRUE)
}

get_added_columns <- function(link, network) {
  project <- link$project
  dep_queries <- get_queries(link)
  input_column_names <- do.call(c, lapply(dep_queries, get_column_order))
  func_output <- execute_function(link = link, network = network)
  output_column_names <- colnames(func_output)
  added_columns <- output_column_names[!output_column_names %in% input_column_names]
  added_columns
}

flatten_list <- function(li) {
  if(length(li) == 0) return(li)
  outer <- length(li)
  li_return <- list()
  c <- 1
  for (o in seq(outer)) {
    inner <- length(li[[o]])
    for (i in seq(inner)) {
      li_return[[c]] <- li[[o]][[i]]
      c <- c + 1
    }
  }
  li_return
}
