register <- function(quosure, type, module, project, network, ...) {
  link <- create_link(quosure = quosure,  project = project, network = network, ... = ...)
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


is_valid_link <- function(link) {
  inherits(link, what = "xafty_link")
}
