
scope <- function(data, link, mask) {
  dependend_args <- get_queries(link, which = "xafty_query", temper = FALSE)
  colnames_dataset <- colnames(data)

  # This part rescopes columns again that were used as the input of the link-function, it also checks the mask object whether a column
  # is masked
  for (query_list in dependend_args) {
    projects <- get_projects(query_list)
    for (project in projects) {
      selection <- query_list[[project]]$select
      for (col in selection) {
        pos_col <- which(colnames_dataset %in% col)
        if(project %in% mask[[col]]) {
          # TODO: This can be perhaps replaced instead of the first project with a more cleanly prepared name
          projects_masked <- mask[[col]]
          scoped_name <- paste0(projects_masked[1], ".", col)
        } else {
          scoped_name <- paste0(project, ".", col)
        }
        colnames_dataset[pos_col] <- scoped_name
      }
    }
  }

  # This part scopes the newly added columns
  variables <- link$variables
  project <- link$project
  for (var in variables) {
    pos_col <- which(colnames_dataset %in% var)
    if(length(pos_col) <= 0) stop(paste0("Variable '", var, "' in project '", project, "' is not present in the data.",
                                         " Perhaps the variable was registered with the wrong name?"))
    scoped_name <- paste0(project, ".", var)
    colnames_dataset[pos_col] <- scoped_name
  }
  colnames(data) <- colnames_dataset
  data
}

unscope <- function(data, link, arg_name, mask) {
  colnames_data <- colnames(data)
  query_list <- link$args[[arg_name]]
  for (query in query_list) {
    selection <- query$select
    project <- query$from
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
  if(length(query) <= 0) return(character(0))
  li <- list()
  for (i in seq_along(query)) {
    project_selection <- query[[i]]
    project <- project_selection$from
    selection <- project_selection$select
    li[[i]] <- paste0(project, ".", selection)
  }
  do.call(c, li)
}

get_scoped_function_order <- function(query, network) {
  columns <- get_column_order(query)
  projects <- get_project_order(query)
  if(inherits(query, "xafty_object_query")) {
    functions <- get_chatty_func_name_from_network(col = get_squared_variable(columns), project = projects, network = network, env_name = "variables")
    return(paste0("object.", projects, ".", functions))
  } else {
    functions <- do.call(c, mapply(get_chatty_func_name_from_network, columns, projects, MoreArgs = list(network = network, env_name = "variables"),
                                   SIMPLIFY = FALSE, USE.NAMES = TRUE))
    return(paste0(projects, ".", functions))
  }
}

get_project_order <- function(query) {
  if(length(query) <= 0) return(character(0))
  li <- list()
  for (i in seq_along(query)) {
    project_selection <- query[[i]]
    project <- rep(project_selection$from, length(project_selection$select))
    li[[i]] <- project
  }
  do.call(c, li)
}

get_column_order <- function(query) {
  if(length(query) <= 0) return(character(0))
  cols_vec <- do.call(c, lapply(query, \(q) q$select))
  names(cols_vec) <- NULL
  cols_vec
}

return_unscoped_data <- function(data, query, dag) {
  if(inherits(query, "xafty_object_query")) return(data)
  data_cols <- colnames(data)
  data_select <- interpolate_masks(query = query, mask = dag$masked_columns, data_cols = data_cols)
  data_selected <- data[data_select]
  colnames(data_selected) <- get_column_order(query)
  data_selected
}

get_masked_column_names <- function(link) {
  queries <- get_queries(link)
  vec_projects <- do.call(c, lapply(queries, get_project_order))
  names(vec_projects) <- NULL
  vec_columns <- do.call(c, lapply(queries, get_column_order))
  names(vec_columns) <- NULL
  duplicated_columns <- vec_columns[duplicated(vec_columns)]
  sapply(duplicated_columns, \(col) vec_projects[vec_columns == col], simplify = FALSE, USE.NAMES = TRUE)
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
