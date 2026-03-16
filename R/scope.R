
scope <- function(data, link, mask) {
  dependend_args <- get_queries(link)
  merged_queries <- do.call(merge_queries, dependend_args)
  colnames_dataset <- colnames(data)
  # This part rescopes columns again that were used as the input of the link-function, it also checks the mask object whether a column
  # is masked
  projects <- get_projects(merged_queries)
  for (project in projects) {
    query <- merged_queries[[project]]
    selection <- merged_queries[[project]]$select
    rename <- fill_rename(query = merged_queries[[project]])
    for (i in seq_along(selection)) {
      col <- rename[i]
      sel <- selection[i]
      pos_col <- which(colnames_dataset %in% col)
      # If pos_col is length 0 then the variable was already scoped from a joined project
      if(project %in% mask[[sel]]) {
        # TODO: This can be perhaps replaced instead of the first project with a more cleanly prepared name
        projects_masked <- mask[[sel]]
        scoped_name <- paste0(projects_masked[1], ".", sel)
      } else {
        scoped_name <- paste0(project, ".", sel)
      }
      colnames_dataset[pos_col] <- scoped_name
    }
  }
  # This part scopes the newly added columns
  variables <- link$variables
  project <- link$project
  for (var in variables) {
    pos_col <- which(colnames_dataset %in% var)
    if(length(pos_col) <= 0) stop(paste0("Function '", link$fun_name, "' in project '", project,
                                         "' is expected to add the following variable(s): ",
                                         paste0(variables, collapse = ", "), ". ",
                                         "However, variable '", var, "' does not appear in the return value of '", link$fun_name,"'.",
                                         " Was the variable perhaps registered with the wrong name?"))
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
    rename <- get_column_order(list(query))
    for (i in seq_along(selection)) {
      col <- selection[i]
      col_scoped <- paste0(project, ".", col)
      if(!col_scoped %in% colnames_data) {
        mask_proj <- mask[[col]]
        scopes <- paste0(mask_proj, ".", col)
        interpolated_mask <- scopes[which(scopes %in% colnames_data)]
        colnames_data[colnames_data == interpolated_mask] <- col_scoped
      }
      colnames_data[colnames_data == col_scoped] <- rename[i]
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
  links <- flatten_list(lapply(query, get_links, network = network))
  codes <- vapply(links, build_fun_code, FUN.VALUE = character(1))
  codes
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
  cols_vec <- unlist(lapply(query, \(q) q$select), use.names = FALSE, recursive = FALSE)
  cols_return <- character(length(cols_vec))
  pos <- 1
  for (i in seq_along(query)) {
    q <- query[[i]]
    vars <- q$select
    n_vars <- length(vars)
    rename_vec <- q$rename
    if(is.null(rename_vec)) {
      pos2 <- pos + n_vars - 1
      cols_return[pos:pos2] <- vars
      pos <- pos2 + 1
    } else {
      for (j in seq_along(vars)) {
        cols_return[pos] <- if (rename_vec[j] == "") vars[j] else rename_vec[j]
        pos <- pos + 1
      }
    }
  }
  cols_return
}

return_unscoped_data <- function(data, query, dag) {
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
  vec_columns <- unlist(lapply(queries, \(query) unlist(lapply(query, \(q) q$select), use.names = FALSE, recursive = FALSE)))
  names(vec_columns) <- NULL
  duplicated_columns <- vec_columns[duplicated(vec_columns)]
  sapply(duplicated_columns, \(col) vec_projects[vec_columns == col], simplify = FALSE, USE.NAMES = TRUE)
}

interpolate_masks <- function(query, mask, data_cols) {
  data_select <- get_scoped_column_order(query)
  cols_present <- data_select %in% data_cols
  if(all(cols_present)) return(data_select)

  column_order <- unlist(lapply(query, \(q) q$select), use.names = FALSE, recursive = FALSE)
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

fill_rename <- function(query) {
  select <- query$select
  rename <- query$rename
  if(is.null(rename)) return(select)
  for (i in seq_along(rename)) {
    if(rename[i] == "") rename[i] <- select[i]
  }
  rename
}
