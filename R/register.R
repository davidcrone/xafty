register <- function(quosure, type, module, project, network, ...) {
  unpacked <- unpack(quosure = quosure, network = network, project = project)
  add_to_ruleset(item = unpacked, module = module, env = network, project = project)
  add_to_network(item = unpacked, network = network, project = project)
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

