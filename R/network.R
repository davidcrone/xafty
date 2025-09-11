
add_to_network <- function(item, network, project, ...) {
  .dots <- list(...)
  project_env <- network[[project]]
  fun_name <- item$fun_name

  if("object_name" %in% names(.dots)) {
    added_object <- get_squared_variable(item$added_object)
    assign(added_object, fun_name, envir = project_env$objects)
  }
  added_columns <- item$added_columns
  for (new_col in added_columns) {
    assign(new_col, fun_name, envir = project_env$variables)
  }
  added_joins <- get_ordered_join_pairs(link = item)
  for (new_join in added_joins) {
    from <-  new_join[1]
    to <- new_join[2]
    assign(to, fun_name, envir = network[[from]]$joined_projects)
  }
  invisible(network)
}
