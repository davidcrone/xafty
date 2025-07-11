
scope <- function(data, link, mask) {

  dependend_args <- link$network$dependencies
  colnames_dataset <- colnames(data)
  for (dep_arg in dependend_args) {
    projects <- names(dep_arg$cols)
    for (project in projects) {
      selection <- dep_arg$cols[[project]]$select
      for (col in selection) {
        pos_col <- which(colnames_dataset %in% col)
        scoped_name <- paste0(project, ".", col)
        colnames_dataset[pos_col] <- scoped_name
      }
    }
  }
  added_columns <- link$network$output$added_cols
  project <- link$info$project

  for (col in added_columns) {
    pos_col <- which(colnames_dataset %in% col)
    scoped_name <- paste0(project, ".", col)
    colnames_dataset[pos_col] <- scoped_name
  }
  colnames(data) <- colnames_dataset
  data
}

unscope <- function(data, link, arg_name, mask) {
  colnames_data <- colnames(data)
  dependencies <- link$network$dependencies[[arg_name]]$cols
  dep_projects <- names(dependencies)
  for (project in dep_projects) {
    selection <- dependencies[[project]]$select
    for (col in selection) {
      col_scoped <- paste0(project, ".", col)
      if(!col_scoped %in% colnames_data) {
        mask_proj <- mask[[col]]
        scopes <- paste0(mask_proj, ".", col)
        interpolated_mask <- scopes[which(scopes %in% colnames_data)]
        colnames_data[colnames_data == interpolated_mask] <- col_scoped
      }
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

return_unscoped_data <- function(data, query, sm) {
  data_cols <- colnames(data)
  data_select <- interpolate_masks(query = query, mask = sm$get_masked_columns(), data_cols = data_cols)
  # TODO Resolve Joined columns
  data_selected <- data[data_select]
  colnames(data_selected) <- get_column_order(query)
  data_selected
}

get_masked_column_names <- function(link) {
  arg_names <- link$network$arg_defs$names
  arg_defs <- link$network$arg_defs$links

  arg_names_query <- arg_names[arg_defs == "xafty_query"]

  vec_projects <- do.call(c, lapply(arg_names_query, \(arg) {
    xafty_query <- link$ruleset$args[[arg]]
    get_project_order(xafty_query)
  }))

  vec_columns <- do.call(c, lapply(arg_names_query, \(arg) {
    xafty_query <- link$ruleset$args[[arg]]
    get_column_order(xafty_query)
  }))

  duplicated_columns <- vec_columns[duplicated(vec_columns)]

  sapply(duplicated_columns, \(col) {
    masked_projects <- vec_projects[vec_columns == col]
    masked_projects
  }, simplify = FALSE, USE.NAMES = TRUE)
}

interpolate_masks <- function(query, mask, data_cols) {
  data_select <- get_scoped_column_order(query)
  cols_present <- data_select %in% data_cols
  if(all(cols_present)) return(data_select)

  column_order <- get_column_order(query)
  project_order <- get_project_order(query)
  vec_columns <- column_order[!cols_present]
  vec_projects <- project_order[!cols_present]

  fill_vec <- vector(mode = "character", length = length(vec_columns))

  for (i in seq_along(vec_columns)) {
    col <- vec_columns[i]
    proj <- vec_projects[i]

    mask_proj <- mask[[col]]

    scopes <- paste0(mask_proj, ".", col)
    interpolated_mask <- scopes[which(scopes %in% data_cols)]
    fill_vec[i] <- interpolated_mask
  }
  data_select[!cols_present] <- fill_vec
  data_select
}
