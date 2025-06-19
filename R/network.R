
add_to_network_variables <- function(item, env, project) {
  function_name <- names(item)
  network_values <- item[[function_name]]$network
  new_cols <- network_values$push
  for (new_col in new_cols) {
    assign(new_col, network_values, envir = env[[project]]$variables)
  }
  env
}

add_to_network_joined_projects <- function(item, env, into, join) {
  function_name <- names(item)
  network_values <- item[[function_name]]$network
  assign(join, network_values, envir = env[[into]]$joined_projects)
  env
}

