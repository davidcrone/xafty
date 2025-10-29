#' Print a xafty Network
#' @description
#' S3 method to print a xafty network.
#' @param x an object with the class "xafty_network"
#' @param ... further arguments passed to or from other methods.
#' @export
print.xafty_network <- function(x, ...) {
  network_name <- x$settings$network_name
  values_in_x <- names(x)
  projects <- x$settings$projects$print_order$project
  cat(paste0("Number of projects in network '", network_name, "': ", length(projects), "\n"))
  if(length(projects) > 0) {
    cat("\n")
  } else {
    cat("\U1F4A1 ", "\033[3mHint: Add a project to your network like this: network$add_project(\"my_project_name\")\033[0m\n", sep = "")
  }
  for(i in seq_along(projects)) {
    project <- projects[i]
    variable_names <- names(x[[project]]$variables)
    joined_projects <- names(x[[project]]$joined_projects)
    variables_classified <- vapply(variable_names, \(name) get_variable_link_type(name = name, project = project, network = x), FUN.VALUE = character(1))
    variables <- variable_names[variables_classified == "query_link"]
    context <- variable_names[variables_classified == "context_link"]
    objects <- variable_names[variables_classified == "object_link"]

    cat(paste0("Project: ", project, "\n"))
    if(length(variables) > 0) {
      variables_print <- paste0(variables, collapse = ", ")
      cat(paste0("Variables: ", variables_print, "\n"))
    }
    if(length(context) > 0) {
      context_print <- paste0(context, collapse = ", ")
      cat(paste0("Context: ", context_print, "\n"))
    }
    if(length(objects) > 0) {
      objects_print <- paste0(paste0("[", objects, "]"), collapse = ", ")
      cat(paste0("Objects: ", objects_print, "\n"))
    }
    if(length(joined_projects) > 0) {
      joins_print <- paste0(joined_projects, collapse = ", ")
      cat(paste0("Joins: ", joins_print, "\n"))
    }
    if(i < length(projects)) cat("\n")
  }
}

eval_args <- function(link, network) {
  xo <- get_xafty_objects_vec(link)
  mapply(evaluate_arg, link$args, xo, MoreArgs = list(network = network), SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

evaluate_arg <- function(arg, xo, network) {
  if(xo %in% c("xafty_object", "xafty_query")) {
    nascent(network, arg)
  } else if (xo == "xafty_state") {
    get_default_state(name = arg, network_env = network)
  } else {
    arg
  }
}

execute_function <- function(link, network) {
  do.call(link$fun, eval_args(link, network))
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

#unused
validate_link_type <- function(link_type, unpacked) {
  if (!"data.frame" %in% unpacked$output$class) stop(paste0("Return value of function '", unpacked$fun_name, "' must be a data.frame"))
  if (link_type == "add") {
    if(!"data.frame" %in% unpacked$first$class) stop(paste0("When registering a function with link type 'add', the first argument of function '", unpacked$fun_name,"' must be a data.frame."))
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

build_dependency_codes <- function(link, network, dag_sm) {
  queries <- get_queries(link, which = c("xafty_query"), temper = FALSE)
  fun_code <- build_fun_code(link)
  # Early termination of function execution for a root node
  if (length(queries) == 0) {
    root_node <- setNames(list(character(0)), fun_code)
    return(root_node)
  }
  # This splits queries from object queries which need a different prefix
  function_codes <- unique(do.call(c, lapply(queries, get_scoped_function_order, network = network)))
  # TODO: link link$joins$projects may need re computation with project_needs_join when a variable name has been interpolated
  #  -> This would also make a differentiation necessary between a link that has been "tampered" with and one who was not
  join_codes <- character(length(link$joins$projects))
  for (i in seq_along(link$joins$projects)) {
    projects <- link$joins$projects[[i]]
    join_id <- paste0("join.", paste0(sort(projects), collapse = "."))
    # This is later used to resolve the join
    dag_sm$set_join(id = join_id, projects = projects)
    dag_sm$set_join_projects(projects = projects)
    join_codes[i] <- join_id
  }
  link_dependencies <- c(function_codes, join_codes)
  node <- setNames(list(link_dependencies), fun_code)
  node
}

interpolate_link_queries <- function(link, state_list = NULL, network) {
  interpolated_args <- get_queries(link, temper = TRUE, network = network, state_list = state_list)
  if(length(interpolated_args) == 0) return(link)
  arg_names <- names(interpolated_args)
  for (arg_name in arg_names) {
    link$args[[arg_name]] <- interpolated_args[[arg_name]]
  }
  link
}

find_xafty_objects <- function(arg) {
  if(is_state_variable(arg)) return("xafty_state")
  if(inherits(arg, "xafty_object_query")) return("xafty_object")
  if("xafty_query_list" %in% class(arg)) return("xafty_query")
  "none_xafty_object"
}

get_xafty_objects_vec <- function(link) {
  args <- link$args
  xafty_objects_vec <- vapply(args, find_xafty_objects, FUN.VALUE = character(1))
  xafty_objects_vec
}

get_queries <- function(link, which = c("xafty_query", "xafty_object"), temper = FALSE, state_list = NULL, network = NULL) {
  xafty_objects_vec <- get_xafty_objects_vec(link)
  arg_names_w_query <- names(xafty_objects_vec)[xafty_objects_vec %in% which]
  if(length(arg_names_w_query) <= 0) return(list())
  arg_w_query <- sapply(arg_names_w_query, \(arg_name) link$args[[arg_name]], simplify = FALSE, USE.NAMES = TRUE)
  if (temper) {
    if(is.null(network)) stop("To temper a query, a network needs to be provided")
    arg_w_query <- sapply(arg_w_query, interpolate_state_in_query, state_list = state_list, network_env = network,
                          simplify = FALSE, USE.NAMES = TRUE)
  }
  arg_w_query
}

is_curly_character <- function(arg) {
  grepl("^\\{[^{}]+\\}$", arg)
}

is_squared_variable <- function(arg) {
  grepl("^\\[(?:[A-Za-z0-9_.]|\\{[A-Za-z0-9_.]+\\})+\\]$", arg, perl = TRUE)
}

get_braced_variable <- function(arg) {
  out <- sub(".*\\{([^{}]+)\\}.*", "\\1", arg)
  out[!grepl("\\{[^{}]+\\}", arg)] <- NA_character_
  out
}

get_squared_variable <- function(arg) {
  match <- gsub("^\\[|\\]$", "", arg)
  match
}

is_valid_variable_name <- function(match) {
  is_valid_variable_name <- identical(match, make.names(match))
  is_valid_variable_name
}

contains_state <- function(name) {
  grepl("\\{[^}]+\\}", name)
}

is_state_variable <- function(arg) {
  if(!is.character(arg) || length(arg) != 1) return(FALSE)
  if(!contains_state(arg)) return(FALSE)
  match <- get_braced_variable(arg)
  is_valid_variable_name(match = match)
}

is_object_variable <- function(arg) {
  if(!is.character(arg) || length(arg) != 1) return(FALSE)
  if(!is_squared_variable(arg)) return(FALSE)
  match <- get_squared_variable(arg)
  is_valid_variable_name(match)
}

get_ordered_join_pairs <- function(link) {
  projects <- get_lead_projects(link)
  # This filters objects from the joined projects. Objects are currently only meant as "end-products" or "helpers"
  # not as intermediary steps that should be used in a xafty query pipeline.
  # Using objects as building steps in a xafty pipeline would break its underlying logic, since objects are a snapshot of
  # a certain query while normal xafty queries are meant to add logic to a variable data.frame
  xafty_objects <- get_xafty_objects_vec(link)
  projects <- projects[vapply(names(projects),
                              \(arg_name) xafty_objects[names(xafty_objects) == arg_name] == "xafty_query",
                              FUN.VALUE = logical(1))]
  n <- length(projects)
  pairs <- list()
  if(n <= 1) return(pairs)
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

build_executable_args <- function(link, data_sm, mask, default_states) {
  args <- link$args
  executable_args <- list()
  object_types <- get_xafty_objects_vec(link)
  for (i in seq_along(args)) {
    xo <- object_types[i]
    arg <- args[i]
    arg_name <- names(arg)
    if(xo == "xafty_query") {
      query_list <- arg[[arg_name]]
      project <- get_lead_project(query_list = query_list)
      data <- data_sm$get_data(project = project)
      data <- unscope(data = data, link = link, arg_name = arg_name, mask = mask)
    } else if (xo == "xafty_object") {
      object_query <- arg[[arg_name]]
      object_key <- paste0(object_query[[1]]$from, ".", get_squared_variable(object_query[[1]]$select))
      data <- data_sm$get_object(object_key)
    } else if (xo == "xafty_state") {
      name <- args[[arg_name]]
      data <- data_sm$get_state(name)
      if(is.null(data)) data <- get_default_state(name = name, network_env = default_states)
    } else if (xo == "none_xafty_object") {
      data <- args[[arg_name]]
    }
    executable_args[[arg_name]] <- data
  }
  executable_args
}

get_lead_projects <- function(link, which = c("xafty_query", "xafty_object")) {
  queries <- get_queries(link, which = which)
  arg_w_query <- names(queries)
  # The lead project will always be the first project in a query
  vapply(arg_w_query, \(arg_name) queries[[arg_name]][[1]]$from, FUN.VALUE = character(1))
}

get_lead_project <- function(query_list) {
  query_list[[1]]$from
}

get_added_variables <- function(link, network) {
  project <- link$project
  dep_queries <- get_queries(link, temper = TRUE, network = network)
  input_column_names <- do.call(c, lapply(dep_queries, get_column_order))
  func_output <- execute_function(link = link, network = network)
  output_column_names <- colnames(func_output)
  added_variables <- output_column_names[!output_column_names %in% input_column_names]
  added_variables
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

list_depth <- function(x) {
  if (!is.list(x)) {
    return(0L)
  }
  if (length(x) == 0) {
    return(1L)
  }
  return(1L + max(sapply(x, list_depth)))
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

is_query_link <- function(link) {
  inherits(link, "query_link")
}

is_context_link <- function(link) {
  inherits(link, "context_link")
}

is_object_link <- function(link) {
  inherits(link, "object_link")
}

build_fun_code <- function(link) {
  paste0(link$project, ".", link$fun_name)
}

bfs_traversal <- function(graph, start, end) {
  # Breadth-First Search
  visited <- list()
  queue <- list(list(node = as.character(start), path = as.character(c(start))))
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    node <- current$node
    path <- current$path
    if (node == as.character(end)) {
      return(path)
    }
    if (is.null(visited[[node]])) {
      visited[[node]] <- TRUE
      neighbors <- graph[[node]]
      for (neighbor in neighbors) {
        if (is.null(visited[[neighbor]])) {
          queue <- append(queue, list(list(node = neighbor, path = c(path, neighbor))))
        }
      }
    }
  }
  return(NULL)
}

get_default_state <- function(name, network_env) {
  if(is_curly_character(name)) {
    name <- get_braced_variable(name)
  }
  existing_states <- names(network_env$states)
  state_registered <- name %in% existing_states
  if(!state_registered) return(network_env$settings$state$global_default)
  network_env$states[[name]]$default
}

# Expects merged queries
project_needs_join <- function(project, query_list, network) {
  links <- lapply(query_list, get_links, network = network)
  deps_projects <- names(links)
  # If no project is dependent on the link's project, the link is only dependent on other projects
  # which means the link's project must not be joined
  if(!project %in% deps_projects) return(FALSE)
  links_ <- links[[project]]
  query_list <- flatten_list(remove_empty_lists(lapply(links_, get_queries,
                                                       which = "xafty_query", temper = TRUE, network = network)))
  query_list <- do.call(merge_queries, query_list)
  # query is depended on a root node for that project and therefore needs to be joined
  if(length(query_list) == 0) return(TRUE)
  project_needs_join(project = project, query_list = query_list, network = network)
}

check_link_type <- function(link) {
  if (is_query_link(link)) return("query_link")
  if (is_object_link(link)) return("object_link")
  if (is_context_link(link)) return("context_link")
  "unknown_link"
}

get_variable_link_type <- function(name, project, network) {
  link <-  get_chatty_link_from_network(name = name, project = project, network = network)
  check_link_type(link)
}

remove_context_queries <- function(query_list) {
  keep <- !vapply(query_list, \(e) inherits(e, "context_query"), logical(1))
  query_list <- query_list[keep]
  query_list
}
