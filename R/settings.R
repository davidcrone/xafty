settings <- function(name) {
  if(length(name) == 0 | length(name) > 1) stop("Please enter the network's name.")
  if(!identical(name, make.names(name))) stop("Please enter a valid name")
    list(
      name = name,
      state = list(
        global_default = NULL
      ),
      # Data.Frame must have zero rows upon creation
      projects = list(
        print_order = data.frame(project = character(0), order = integer(0), root = character(0), info = character(0))
      )
    )
}

#' Set or Change the Print Order of Projects
#'
#' @description
#' This function allows you to manually define or update the display order of projects within a `xafty` network.
#' You can specify the print order for individual projects or for multiple projects simultaneously by providing a numeric
#' vector of corresponding order values. If no order vector is provided, each listed project will be added sequentially
#' to the end of the existing order.
#'
#' @param projects A character vector containing the names of the projects whose print order should be set or updated.
#' @param order A numeric vector of the same length as `projects`, specifying the desired print order for each project.
#' If `NULL` (the default), each project will be appended to the end of the current order.
#' @param network A `xafty` network object containing the projects.
#'
#' @returns
#' The updated `xafty` network (returned invisibly).
#'
#' @details
#' If a specified project already exists in the print order, its order value will be updated.
#' If a project is not yet part of the print order but exists in the network, it will be added with the specified or automatically assigned order.
#' An error is raised if any project listed in `projects` does not exist in the given network.
#'
#' @examples
#' xafty_network <- init_network("change_order",
#'                    projects = c("Project2", "Project1"))
#' print(xafty_network) # Prints Project2 first, then Project1
#'
#' # Change the print order
#' set_project_print_order(
#'   projects = c("Project2", "Project1"),
#'   order = c(2, 1),
#'   network = xafty_network
#' )
#' print(xafty_network)
#'
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
      # Add newly created project to print order df
      df_single <- create_single_order_df(df_projects = df_project_order, project = project, order = ord)
      df_project_order <- rbind(df_project_order, df_single)
    }
  }
  df_project_order_ordered <- df_project_order[order(df_project_order$order), ]
  network$settings$projects$print_order <- df_project_order_ordered
  invisible(network)
}

create_single_order_df <- function(df_projects, project, order) {
  coln <- colnames(df_projects)
  mtrx <- as.data.frame(matrix(ncol = length(coln)))
  colnames(mtrx) <- coln
  mtrx$project <- project
  mtrx$order <- order
  mtrx
}

set_project_info <- function(project, info = NULL, network) {
  df_order <- network$settings$projects$print_order
  if(is.null(info)) info <- NA_character_
  df_order$info[df_order$project == project] <- info
  network$settings$projects$print_order <- df_order
  network
}
