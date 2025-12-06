
# Handles correct context for on_entry and on_exit
clean_wrapper <- function(project, order, dag, contexts, network) {
  entry_funcs <- network[[project]]$wrappers$on_entry
  exit_funcs <- network[[project]]$wrappers$on_exit
  # Early exit when no wrapper functions are available
  if (length(entry_funcs) == 0 && length(exit_funcs) == 0) return(order)
  suffix <- paste0(project, ".")
  is_project <- which(startsWith(order, suffix))

  start_pos <- min(is_project)
  end_pos <- max(is_project)
  projects_extract <- order[start_pos:end_pos]

  is_project_sub <- startsWith(projects_extract, suffix)
  # Early exit when all projects have no foreign project between variable and wrapper functions
  if(all(is_project_sub)) return(order)
  entry_nodes <- if (!is.null(entry_funcs)) paste0(project, ".", entry_funcs) else character(0)
  exit_nodes  <- if (!is.null(exit_funcs))  paste0(project, ".", exit_funcs)  else character(0)


  # Handle on exit nodes which may depend on a foreign function
  dependend_foreign <- get_upstream_dependencies(project = project, dag = dag[projects_extract], targets = exit_nodes, stop_at = character(0))

  # Remove all on entry and on exit functions in order to begin rebuilding context from a clean slate
  extract <- projects_extract[!projects_extract %in% c(entry_nodes, exit_nodes)]


  start <- if (start_pos > 1) order[1:(start_pos - 1)] else character(0)
  end   <- if (end_pos < length(order)) order[(end_pos + 1):length(order)] else character(0)

  if(length(dependend_foreign) > 0) {
    is_polluted_context <- any(startsWith(dependend_foreign, suffix))
    # remove depndency from extract and move to before context
    if(!is_polluted_context) {
      extract <- extract[!extract %in% dependend_foreign]
      start <- c(start, dependend_foreign)
    } else {
      # TODO: foreigns need to remain in the context and might be temporary cloaked with the prefix 'project.' to keep them at place
    }
  }
  # Package order into building blocks for easy resolving of wrappers
  packages <- package_wrapper(project = project, extract = extract)

  if(length(packages$packages) == 0) {
    order <- c(start, packages$start, packages$end, end)
    return(rebuild_context(project = project, order = order, on_entry = entry_nodes, on_exit = exit_nodes))
  }
  for (i in seq_along(packages$packages)) {
    package <- packages$packages[[i]]
    foreign <- interpolate_contexts(order = package$foreign, contexts = contexts)
    projects <- package$project
    deps <- unlist(lapply(projects, \(p) dag[[p]]))
    needed <- any(deps %in% foreign)
    if(!needed) {
      packages$packages[[i]]$foreign <- character(0)
      # propagate
      if(i < length(packages$packages)) {
        packages$packages[[i + 1]]$foreign <- c(foreign, packages$packages[[i + 1]]$foreign)
        # bring foreigns to end if they are not needed
      } else {
        packages$end <- c(foreign, packages$end)
      }
    }
  }

  extract_reorder <- unlist(lapply(packages$packages, \(package) c(package$foreign, package$project)))

  order <- c(start, packages$start, extract_reorder, packages$end, end)
  rebuild_context(project = project, order = order, on_entry = entry_nodes, on_exit = exit_nodes)
}

projects_with_context <- function(projects, network) {
  has_on_entry <- vapply(projects, \(project) !is.null(network[[project]]$wrappers$on_entry), FUN.VALUE = logical(1))
  has_on_exit <- vapply(projects, \(project) !is.null(network[[project]]$wrappers$on_exit), FUN.VALUE = logical(1))
  project_w_contect <- unique(c(projects[has_on_entry], projects[has_on_exit]))
  project_w_contect
}


clean_all_wrappers <- function(projects, order, dag, network) {
  # Step 1: find all context ranges (entry .. exit)
  projects <- projects_with_context(projects = projects, network = network)
  if(length(projects) <= 0) return(order)

  li_ranges <- lapply(projects, function(p) {
    project. <- paste0(p, ".")
    project_start <- startsWith(order, project.)
    if (any(project_start)) {
      project_pos <- which(project_start)
      list(project = p, start = min(project_pos), end = max(project_pos))
    } else NULL
  })
  li_ranges <- Filter(Negate(is.null), li_ranges)
  # Step 2: find the *innermost* context (smallest range that’s not overlapping others)
  ranges <- do.call(rbind, lapply(li_ranges, function(x) cbind(x$project, x$start, x$end)))
  colnames(ranges) <- c("project", "start", "end")
  ranges <- as.data.frame(ranges, stringsAsFactors = FALSE)
  ranges$start <- as.numeric(ranges$start)
  ranges$end   <- as.numeric(ranges$end)
  ranges$span  <- ranges$end - ranges$start

  ranges <- ranges[order(ranges$span, decreasing = FALSE), ]
  projects <- ranges$project
  contexts <-  NULL
  for (project in projects) {
    order <- clean_wrapper(project = project, order = order, dag = dag, contexts = contexts, network = network)
    packed_wrappers <- pack_project_wrappers(project = project, order = order, contexts = contexts)
    order <- packed_wrappers$order
    contexts <- packed_wrappers$contexts
  }
  interpolate_contexts(order = order, contexts = contexts)
}

pack_project_wrappers <- function(project, order, contexts = NULL) {
  if(is.null(contexts)) {
    contexts <- list()
  }
  out <- character()
  context_counts <- list()
  i <- 1
  project. <- paste0(project, ".")
  while (i <= length(order)) {
    val <- order[i]
    if (startsWith(order[i], project.)) {
      # Find matching on_exit for the same project
      j <- i + 1
      while (j <= length(order)) {
        if (startsWith(order[j], project.)) j <- j + 1 else break
      }
      group_block <- order[i:(j - 1)]
      if (is.null(context_counts[[project]])) context_counts[[project]] <- 0
      context_counts[[project]] <- context_counts[[project]] + 1

      ctx_name <- paste0(project, ".context_", context_counts[[project]])
      contexts[[ctx_name]] <- group_block
      out <- c(out, ctx_name)
      i <- j
    } else {
      out <- c(out, val)
      i <- i + 1
    }
  }
  # Return both modified order and contexts
  list(order = out, contexts = contexts)
}

interpolate_contexts <- function(order, contexts) {
  result <- unlist(lapply(order, function(x) {
    if (x %in% names(contexts)) {
      contexts[[x]]
    } else {
      x
    }
  }))
  return(result)
}

# The function brings the wrappers into a form that allows for sequential checking of dependencies of foreigns
# in order to check wether the wrappers need to be broken apart
package_wrapper <- function(project, extract) {
  suffix <- paste0(project, ".")
  is_project <- startsWith(extract, suffix)
  rle_is_project <- rle(is_project)

  # Take position of head of extract where entries are TRUE
  start_pos <- if(rle_is_project$values[1]) {
    seq(1:rle_is_project$lengths[1])
  } else numeric(0)
  # Take tail of all forgein projects from extract
  end_pos <- if(!rle_is_project$values[length(rle_is_project$values)]) {
    n_tail <- rle_is_project$lengths[length(rle_is_project$lengths)]
    n_end <- length(extract)
    n_start <- n_end - n_tail + 1
    seq(n_start, n_end)
  } else numeric(0)

  li_package <- list(
    start = character(0),
    end = character(0),
    packages = list()
  )
  li_package$start = extract[start_pos]
  li_package$end = extract[end_pos]

  package_extract <- extract[!seq(length(extract)) %in% c(start_pos, end_pos)]
  if(length(package_extract) == 0) return(li_package)

  is_project <- startsWith(package_extract, suffix)
  rle_is_project <- rle(is_project)

  sequence <- seq(1, length(rle_is_project$lengths), by = 2)

  start_pos <- 1
  li_temp <- list()
  for (i in sequence) {
    l_foreign <- rle_is_project$lengths[i]
    l_project <- rle_is_project$lengths[i + 1]

    start_seq <- seq(start_pos, start_pos + l_foreign - 1)
    end_seq <-  seq(max(start_seq) + 1, max(start_seq) + l_project)

    foreign <- package_extract[start_seq]
    project <- package_extract[end_seq]
    li_temp[[paste0("s_", i)]] <- list(
      foreign = foreign,
      project = project
    )
    start_pos <- max(end_seq) + 1
  }

  names(li_temp) <- NULL
  li_package$packages <- li_temp
  li_package
}

rebuild_context <- function(project, order, on_entry, on_exit) {
  suffix <- paste0(project, ".")
  rebuilt_context <- character(0)
  opened <- FALSE
  for (i in seq_along(order)) {
    node <- order[i]
    is_context <- startsWith(node, suffix)
    if(is_context && !opened) {
      rebuilt_context <-  c(rebuilt_context, on_entry, node)
      if(i == length(order)) rebuilt_context <- c(rebuilt_context, on_exit)
      opened <- TRUE
    } else if (is_context && opened) {
      rebuilt_context <- c(rebuilt_context, node)
      if(i == length(order)) rebuilt_context <- c(rebuilt_context, on_exit)
    } else if (!is_context && opened) {
      rebuilt_context <- c(rebuilt_context, on_exit, node)
      opened <- FALSE
    } else {
      rebuilt_context <- c(rebuilt_context, node)
    }
  }
  rebuilt_context
}

## Handle dependencies on 'on_exit'

get_upstream_dependencies <- function(project, dag, targets, stop_at = character(0)) {
  if (length(targets) == 0) return(character(0))
  visited <- character(0)
  suffix <- paste0(project, ".")
  # convert NULL dag edges to character(0) safely
  get_deps <- function(node) {
    if (is.null(dag[[node]])) character(0) else dag[[node]]
  }

  relevant <- names(dag)
  stack <- unlist(lapply(targets, \(target) { deps <- get_deps(target); deps[!startsWith(deps, suffix) & deps %in% relevant] }))

  while (length(stack) > 0) {
    node <- stack[[1]]
    stack <- stack[-1]

    # Skip if we already visited this node
    if (node %in% visited) next
    visited <- c(visited, node)

    # If node is in stop_at, do not explore its dependencies (but still include node if discovered)
    if (node %in% stop_at) next

    deps <- get_deps(node)
    if (length(deps) > 0) {
      # push deps to stack (only those not yet visited and relevant to the extracted upstream pipeline)
      new_deps <- deps[!deps %in% visited & deps %in% relevant]
      if (length(new_deps) > 0) stack <- c(new_deps, stack)
    }
  }
  # visited contains targets + their upstream nodes in discovery order.
  result <- unique(visited)
  result
}
