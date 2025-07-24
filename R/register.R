
#' Register a Function in a Network
#' @description
#' The function is the workhorse behind all register methods of a network.
#' @param quosure A function call enquoted with rlang::enquo
#' @param project The project name of the project within the network where the function should be registered.
#' @param network A xafty network.
#' @param module The module name of the rulset. Currently only "link" is supported
#' @param ... Configurations of the register. Following parameters are allowed
#' * added_columns : The parameter takes in a character vector of column names that are added to the data set.
#' @returns A xafty network (invisibly)
#' @export
register <- function(quosure, project, network, module, ...) {
  link <- create_link(quosure = quosure,  project = project, network = network, ... = ...)
  validate_network_integrity(link = link, network = network)
  add_to_ruleset(item = link, module = module, env = network, project = project)
  add_to_network(item = link, network = network, project = project)
  invisible(network)
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

create_link <- function(quosure, project, network, ...) {
  .dots <- list(...)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_info <- list(
    package = get_function_package(func_name = list_args$fun_name),
    language = "R",
    project = project
  )
  link<- append(list_args, list_info)
  class(link) <- c("xafty_link", "list")

  if("added_columns" %in% names(.dots)) {
    link$added_columns <- .dots[["added_columns"]]
  } else {
    link$added_columns <- get_added_columns(link = link, network = network)
  }
  link
}

validate_network_integrity <- function(link, network) {
  queries <- get_queries(link)
  if(length(queries) <= 0) return(invisible(TRUE))
  browser()
  flat_queries <- flatten_list(queries)
  for (query in flat_queries) {
    project <- query$from
    selection <- query$select
    for (col in selection) {
      validate_query(col = col, project = project, network = network)
    }
  }
}

validate_query <- function(col, project, network) {
  project_subset <- network[[project]]
  if(is.null(project_subset)) {
    stop(paste0("Project: ", project, " is not contained in the network"))
  }
  columns_subset <- project_subset$variables[[col]]
  if(is.null(columns_subset)) {
    stop(paste0("Column: ", col, " is not contained in project: ", project))
  }
  invisible(TRUE)
}
