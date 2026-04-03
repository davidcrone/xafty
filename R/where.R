
#' Filter a Network Pipeline
#' @param query_list A xafty query obtained via [`xafty::query`]
#' @param ... Filter conditions, e.g. `col1 >= 1```
#' @returns A xafty query with a where filter applied
#' @export
where <- function(query_list, ...) {
  expr <- rlang::expr(...)
  where_select <- all.vars(expr)
  where_query <- list(
    expr = strip_prefix_from_expr(expr)
  )
  raw_query <- lapply(where_select, \(select) {
    where_query <- split_select(var = select)
    where_query
  })
  class(where_query) <- c("list", "where_expr")
  if(inherits(query_list, "xafty_query")) {
    query_list_classes <- class(query_list$query)
    query_list$query <- append(query_list$query, raw_query)
    query_list$query[[length(query_list$query) + 1]] <- where_query
    class(query_list$query) <- query_list_classes
  } else {
    query_list_classes <- class(query_list)
    query_list <- append(query_list, raw_query)
    query_list[[length(query_list) + 1]] <- where_query
    class(query_list) <- query_list_classes
  }
  query_list
}


strip_prefix_from_expr <- function(expr) {
  # Base case: if it's a symbol, check if it needs stripping
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_string(expr)
    if (grepl("\\.", name)) {
      # Take only the part after the last dot
      new_name <- sub(".*\\.", "", name)
      return(rlang::sym(new_name))
    }
    return(expr)
  }

  # If it's a call (e.g. `>=`, `&`, `==`, function calls...), recurse into each element
  if (rlang::is_call(expr)) {
    new_args <- lapply(as.list(expr), strip_prefix_from_expr)
    return(as.call(new_args))
  }

  # For literals (numbers, strings, logicals, NULL, etc.), return as-is
  expr
}

split_select <- function(var) {
  split <- strsplit(x = var, split = ".", fixed = TRUE)[[1]]
  if(length(split) == 2) {
    where_query <- list(
      select = split[2],
      from = split[1]
    )
    class(where_query) <- c("list", "where_query")
  } else {
    where_query <- list(
      select = split,
      from = "unevaluated"
    )
    class(where_query) <- c("list", "raw_query", "where_query")
  }
  where_query
}

create_where_filter <- function(expr) {
  filter_func <- function(data) {
    idx <- eval(expr, envir = data, enclos = parent.frame())
    data[which(idx), , drop = FALSE]
  }
  filter_func
}

remove_where_query <- function(query_list) {
  has_where <- vapply(query_list, \(query) inherits(query, "where_query") || inherits(query, "where_expr"), FUN.VALUE = logical(1))
  if(!any(has_where)) return(query_list)
  query_list[!has_where]
}

get_where<- function(query_list) {
  is_where <- vapply(query_list, \(query) inherits(query, "where_query") || inherits(query, "where_expr"), FUN.VALUE = logical(1))
  query_list[is_where]
}

get_where_query <- function(query_list) {
  is_where <- vapply(query_list, \(query) inherits(query, "where_query"), FUN.VALUE = logical(1))
  query_list[is_where]
}

remove_where_expr <- function(query_list) {
  has_not_expr<- vapply(query_list, \(query) !inherits(query, "where_expr"), FUN.VALUE = logical(1))
  query_list[has_not_expr]
}

get_where_expr <- function(query_list) {
  has_expr<- vapply(query_list, \(query) inherits(query, "where_expr"), FUN.VALUE = logical(1))
  query_list[has_expr]
}

apply_where_filter <- function(data, dag) {
  if(length(dag$where_query) == 0) return(data)
  expr <- get_where_expr(query_list = dag$where_query)
  qry <- get_where_query(query_list = dag$where_query)
  class(qry) <- c("list", "xafty_query_list")
  filter_func <- create_where_filter(expr[[1]]$expr)
  link <- list(
    func_name = "filter_func",
    fun = filter_func,
    args = list(
      data = qry
    )
  )
  data <- unscope(data = data, link = link, arg_name = "data", mask = dag$masked_columns)
  data <- filter_func(data = data)
  scope(data = data, link = link, mask = dag$masked_columns)
}
