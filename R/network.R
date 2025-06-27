
add_to_network <- function(item, network, project) {
  project_env <- network[[project]]
  fun_name <- item$ruleset$fun_name
  added_columns <- item$network$output$added_cols
  added_joins <- item$network$output$join_pairs
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
