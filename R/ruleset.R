ruleset <- function(link_types = c("link", "object")) {
  sapply(link_types, \(link_type) list())
}

settings <- function(network_name) {
  if(length(network_name) == 0 | length(network_name) > 1) stop("Please enter the network's name.")
  if(!identical(network_name, make.names(network_name))) stop("Please enter a valid network name")
    list(
      network_name = network_name,
      state = list(
        global_default = NULL
      ),
      projects = list(
        print_order = data.frame(project = character(), order = integer())
      )
    )
}

#' Change Print Order of Projects
#' @description
#' The function allows to manually set the order for each project in the network. It is also possible to set the order
#' for several projects at once by providing an equally length order vector.
#' @param projects Character of project names of which the order should be changed
#' @param order Numeric vector with equal length of parameter projects to set the corresponding print order
#' @param network A xafty network containing the projects
#' @examples
#' xafty_network <- init_network("change_order", projects = c("Project2", "Project1"))
#' print(xafty_network) # Prints Project2 first and Project1 second
#' set_project_print_order(projects = c("Project2", "Project1"),
#'                         order = c(2, 1),
#'                         network = xafty_network)
#' print(xafty_network)
#' @returns A xafty network invisibly
#' @export
set_project_print_order <- function(projects, order = NULL, network) {
  if(!is.null(order)) {
    if(!length(projects) == length(order)) {
      stop("Length of parameter 'projects' and 'order' must be the same.")
    }
  }
  df_project_order <- network$settings$projects$print_order
  for (i in seq_along(projects)) {
    project <- projects[i]
    if(is.null(order)) {
      if(nrow(df_project_order) == 0) {
        ord <- 1L
      } else {
        ord <- max(df_project_order$order) + 1
      }
    } else {
      ord <- order[i]
    }
    if(project %in% df_project_order$project) {
      df_project_order$order[df_project_order$project == project] <- ord
    } else {
      if(!project %in% names(network)) stop(paste0("Project '", project, "' is not present in network '", network$settings$network_name, "'."))
      df_single <- data.frame(project = project, order = ord)
      df_project_order <- rbind(df_project_order, df_single)
    }
  }
  df_project_order_ordered <- df_project_order[order(df_project_order$order), ]
  network$settings$projects$print_order <- df_project_order_ordered
  invisible(network)
}
