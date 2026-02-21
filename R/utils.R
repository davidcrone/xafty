#' Print a xafty Network
#' @description
#' S3 method to print a xafty network.
#' @param x an object with the class "xafty_network"
#' @param ... further arguments passed to or from other methods.
#' @export
print.xafty_network <- function(x, ...) {
  network_name <- x$settings$name
  values_in_x <- names(x)
  projects <- x$settings$projects$print_order$project
  cat("---\n")
  cat("\U1F4CA", network_name, "\n")
  cat("\n")
  cat("\U1F332 ", "Projects (", length(projects), "):\n", sep = "")

  if(length(projects) == 0) {
    cat("\U1F4A1 ", "Hint: Add a project to your network like this: network$add_project(\"my_project_name\")\n", sep = "")
  }

  for(i in seq_along(projects)) {
    project <- projects[i]
    print_project_summary(project = project, network = x)
  }
}

print_project_summary <- function(project, network) {
  df_print <- network$settings$projects$print_order
  df_print_project <- df_print[df_print$project == project, ]
  project_env <- network[[project]]
  ruleset <- project_env$ruleset
  contents <- names(project_env$variables)
  variables_classified <- vapply(contents, \(name) get_variable_link_type(name = name, project = project, network = network), FUN.VALUE = character(1))
  variables <- contents[variables_classified == "query_link"]
  contexts <- contents[!variables_classified == "query_link"]

  is_last_project <- which(df_print$project == project) == nrow(df_print)

  indent <- "  "
  if(!is_last_project) {
    downwards <- paste0(indent, " \u2502")
    navigate <- " \u251C" # unclosed project with
  } else {
    downwards <- paste0(indent, indent)
    navigate <- " \u2514"
  }

  print_variables <- paste0(length(variables), "\U1F331")
  print_joins <- paste0(length(project_env$joined_projects), "\U1F517")
  print_contexts <- paste0(length(contexts), "\U1F9E9")

  info_contents <- paste0(c(print_variables, print_joins, print_contexts), collapse = " | ")

  info_text <- if(is.na(df_print_project$info)) "" else paste0(": ", df_print_project$info)

  cat(indent, navigate, "\U1F4C1 ", project, info_text, "\n", sep = "")
  cat(downwards, " ", "\u2514 ", info_contents, "\n")
}

#' Print a xafty Project
#' @description
#' S3 method to print a xafty network.
#' @param x an object with the class "xafty_project"
#' @param ... further arguments passed to or from other methods.
#' @export
print.xafty_project <- function(x, ...) {
  project <- x$settings$name
  variable_names <- names(x$variables)
  contents <- lapply(variable_names, \(name) x$ruleset[[x$variables[[name]]]])
  is_var <- vapply(contents, \(link) is_query_link(link), FUN.VALUE = logical(1))
  variables <- contents[is_var]
  contexts <- contents[!is_var]

  indent <- "  "
  if(length(variables) > 0) {
    cat("\U1F4C1 Project: ", project, "\n", sep = "")
  } else {
    cat("\U1F4C1 Project: ", project, " (empty)\n", sep = "")
  }

  if(length(variables) > 0) {
    raw_layer <- vapply(variables, \(link) link$layer, FUN.VALUE = numeric(1))
    names(raw_layer) <- variable_names
    max_layer <- max(raw_layer)
    layer_vec <- sort(unique(raw_layer))
    for (layer in layer_vec) {
      layer_variables <- paste0(sort(names(raw_layer)[raw_layer == layer]), collapse = ", ")
      is_max <- layer == max_layer
      if(!is_max) layer_close <- "\u251C" else layer_close <- "\u2514"
      if(layer > 0) {
        cat(indent, " ", layer_close, " \U1F6E0 Layer ", paste0(layer ,": "), layer_variables, "\n", sep = "")
      } else {
        cat(indent, " ", layer_close, " \U1F331 Root:", layer_variables, "\n", sep = "")
      }
    }
  }

  cat("\n")

  print_joins(project_env = x, indent = indent)

  cat("\n")
}

print_joins <- function(project_env, indent) {
  joined_projects <- names(project_env$joined_projects)
  if(length(joined_projects) == 0) {
    cat("\U1F517 ", "Joins:", "(None)\n")
    return(invisible(project_env))
  }

  li_raw_pairs <- list()
  cat("\U1F517 ", "Joins (", length(joined_projects), "):\n", sep = "")
  for (i in seq_along(joined_projects)) {
    project <- joined_projects[i]
    cat(indent, " ", "\U27A1\uFE0F ", project, sep = "")
    cat("\n")
    # TODO: Print available projects exposed from the join!
  }
}

eval_args <- function(link, network) {
  xo <- get_xafty_objects_vec(link)
  mapply(evaluate_arg, link$args, xo, MoreArgs = list(network = network), SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

evaluate_arg <- function(arg, xo, network) {
  if(xo %in% c("xafty_object", "xafty_query")) {
    nascent(arg, network)
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

build_dependency_codes <- function(link, network) {
  queries <- get_queries(link, which = c("xafty_query"), temper = FALSE)
  fun_code <- build_fun_code(link)
  # Early termination of function execution for a root node
  if (length(queries) == 0) {
    root_node <- setNames(list(character(0)), fun_code)
    root_return <- list(
      node = root_node,
      joins = list()
    )
    return(root_return)
  }
  # This splits queries from object queries which need a different prefix
  function_codes <- unique(unlist(lapply(queries, get_scoped_function_order, network = network)))
  wrapper_codes <- build_on_entry_dependencies(link = link, network = network, fun_code = fun_code)
  # TODO: link link$joins$projects may need re computation with function: 'project_needs_join' when a variable name has been interpolated
  #  -> This would also make a differentiation necessary between a link that has been "tampered" with and one who was not
  join_code <- character(length(link$joins$projects))
  joins <- list()
  for (i in seq_along(link$joins$projects)) {
    projects <- link$joins$projects[[i]]
    join_id <- paste0("join.", paste0(sort(projects), collapse = "."))
    join_code[i] <- join_id
    joins[[join_id]] <- projects
  }
  link_dependencies <- c(function_codes, join_code, wrapper_codes)
  node <- setNames(list(link_dependencies), fun_code)
  list(
    node = node,
    joins = joins
  )
}

build_on_entry_dependencies <- function(link, network, fun_code) {
  project <- link$project
  group <- link$group
  if(is.null(group)) return(character(0))
  on_entry_funcs <- network[[project]][["groups"]][[group]]$contexts$on_entry
  if(is.null(on_entry_funcs)) return(character(0))
  on_entry_codes <- paste0(group, ".", project, ".", on_entry_funcs)
  # If the fun_code is itself is a wrapper function, it should only get on_entry codes as dependencies
  # that were registered earlier
  if(fun_code %in% on_entry_codes) {
    pos <- which(fun_code == on_entry_codes) - 1
    if(pos == 0) return(character(0)) else on_entry_codes <- on_entry_codes[1:pos]
  }
  on_entry_codes
}

# The function builds the dependencies for on_entry nodes
build_on_entry_node <- function(link, network, dag) {
  project <- paste0(link$group, ".", link$project)
  li_on_entry_node <- build_dependency_codes(link, network = network)
  on_entry_node <- li_on_entry_node$node
  on_entry_code <- names(on_entry_node)
  project_dag <- dag[grepl(paste0("^", link$group, "\\.", link$project, "\\."), names(dag))]
  # Adds foreign project nodes that wrapper nodes depend on; unique is necessary since the package toposort cannot work with
  # duplicated dependencies in a single node
  foreign_deps <- unique(get_all_non_project_codes(project = project, codes = project_dag))
  # removes foreign nodes that depend on wrapper nodes, avoids cycles
  wrapper_node_deps <- filter_targets_without_prefix(project = project, targets = foreign_deps, dag = dag)
  deps <- unique(c(on_entry_node[[on_entry_code]], wrapper_node_deps))
  node <- setNames(list(deps), on_entry_code)
  list(
  node = node,
  joins = li_on_entry_node$joins
  )
}

build_on_exit_node <- function(link, network, dag) {
  project <- paste0(link$group, ".", link$project)
  li_on_exit_node <- build_dependency_codes(link, network = network)
  on_exit_node <- li_on_exit_node$node
  on_exit_code <- names(on_exit_node)
  deps_project <- names(dag)[grepl(paste0("^", link$group, "\\.", link$project, "\\."), names(dag))]

  # Adding on exit functions registered earlier from wrapper project as dependencies
  on_exit_codes <- paste0(project, ".", network[[link$project]][["groups"]][[link$group]]$contexts$on_exit)
  pos <- which(on_exit_code == on_exit_codes) - 1
  if(pos == 0) exit_deps <- character(0) else exit_deps <- on_exit_codes[1:pos]

  deps <- unique(c(on_exit_node[[on_exit_code]], deps_project, exit_deps))
  node <- setNames(list(deps), on_exit_code)
  list(
    node = node,
    joins = li_on_exit_node$joins
  )
}

on_exit_codes <- function(link, network) {
  arg_queries <- get_queries(link)

  li_projects <- lapply(arg_queries, \(query_list) get_projects(query_list))
  projects <- unique(unlist(li_projects))
  li_on_exit_codes <- list()
  for (project in projects) {
    on_exit_funcs <- network[[project]]$wrappers$on_exit
    if(is.null(on_exit_funcs)) {
      on_exit_codes <- NULL
    } else {
      on_exit_codes <- paste0(project, ".", on_exit_funcs)
    }
    li_on_exit_codes[[project]] <- on_exit_codes
  }
  unlist(li_on_exit_codes, use.names = FALSE)
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

get_join_project <- function(link) {
  from <- link$project
  query_lists <- get_queries(link, which = "xafty_query")
  joins <- vapply(query_lists, get_lead_project, character(1), USE.NAMES = FALSE)
  joins_to <- joins[!joins %in% from]
  joins_to
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

get_lead_projects <- function(link, which = "xafty_query") {
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
  if(is.null(link$group)) group <- character(0) else group <- paste0(link$group, ".")
  paste0(group, link$project, ".", link$fun_name)
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
  query_list_ <- lapply(links_, get_queries,
         which = "xafty_query", temper = TRUE, network = network)
  # Root node reached?
  if(has_empty_list(query_list_)) return(TRUE)
  query_list <- flatten_list(remove_empty_lists(query_list_))
  query_list <- do.call(merge_queries, query_list)
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

# Checks whether a link argument has {.data}, this is used when the argument simply needs the data without a certain
# variable
has_.data <- function(link) {
  args <- link$args
  is_character <- vapply(args, \(arg) is.character(arg), FUN.VALUE = logical(1), USE.NAMES = FALSE)
  char_args <- args[is_character]
  vapply(char_args, \(arg) arg == "{.data}", FUN.VALUE = logical(1))
}

build_.data_link <- function(link, node, dag_sm) {
  dep_codes <- unlist(node, use.names = FALSE)
  deps_funcs <- dep_codes[!grepl("^join.", dep_codes)]
  all_links <- dag_sm$get_links()
  dep_links <- lapply(deps_funcs, \(code) all_links[[code]])
  dep_queries <- get_supplied_queries(links = dep_links)
  link$args <- sapply(link$args, \(arg) {
    if(!is.character(arg)) return(arg)
    if(all(arg == "{.data}")) {
      merge_queries(dep_queries)
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
  link
}
