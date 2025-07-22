
add_to_network <- function(item, network, project) {
  project_env <- network[[project]]
  fun_name <- item$fun_name
  added_columns <- item$added_columns
  added_joins <- get_ordered_join_pairs(link = item)
  for (new_col in added_columns) {
    assign(new_col, fun_name, envir = project_env$variables)
  }
  for (new_join in added_joins) {
    from <-  new_join[1]
    to <- new_join[2]
    assign(to, fun_name, envir = network[[from]]$joined_projects)
  }
  invisible(network)
}
