
scope <- function(data, link) {
  # TODO Scope dependencies again!
  browser()
  added_columns <- link$network$output$added_cols
  project <- link$info$project
  colnames_dataset <- colnames(data)

  for (col in added_columns) {
    pos_col <- which(colnames_dataset %in% col)
    scoped_name <- paste0(project, ".", col)
    colnames_dataset[pos_col] <- scoped_name
  }
  colnames(data) <- colnames_dataset
  data
}

unscope <- function(data, link, arg_name) {
  colnames_data <- colnames(data)
  dependencies <- link$network$dependencies[[arg_name]]$cols
  dep_projects <- names(dependencies)
  for (project in dep_projects) {
    selection <- dependencies[[project]]$select
    for (col in selection) {
      col_scoped <- paste0(project, ".", col)
      colnames_data[colnames_data == col_scoped] <- col
    }
  }
  colnames(data) <- colnames_data
  data
}

get_scoped_column_order <- function(query) {
  li <- list()
  for (i in seq_along(query)) {
    project_selection <- query[[i]]
    project <- project_selection$from
    selection <- project_selection$select
    li[[i]] <- paste0(project, ".", selection)
  }
  do.call(c, li)
}

get_project_order <- function(query) {
  li <- list()
  for (i in seq_along(query)) {
    project_selection <- query[[i]]
    project <- rep(project_selection$from, length(project_selection$select))
    li[[i]] <- project
  }
  do.call(c, li)
}

get_column_order <- function(query) {
  cols_vec <- do.call(c, lapply(query, \(q) q$select))
  names(cols_vec) <- NULL
  cols_vec
}

return_unscoped_data <- function(data, query) {
  data_select <- get_scoped_column_order(query)
  # TODO Resolve Joined columns
  data_selected <- data[data_select]
  colnames(data_selected) <- get_column_order(query)
  data_selected
}
