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

list_graph_to_edges <- function(dag) {
  dag_nodes <- dag$dag
  edgelist <- lapply(seq_along(dag_nodes), \(i) {
    node <- dag_nodes[i]
    node_name <- names(node)
    from <- node[[node_name]]
    if(length(from) <= 0) return(NULL)
    to <- rep(node_name, length(from))
    data.frame(
      from = from,
      to = to
    )
  })
  do.call(rbind, edgelist)
}
