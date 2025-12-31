
where <- function(query_list, ...) {
  expr <- rlang::expr(...)
  where_select <- all.vars(expr)
  raw_query <- query(lapply(where_select, \(s) s))
  where_query <- list(
    expr = expr
  )
  raw_query <- lapply(raw_query, \(query) {class(query) <- c("list", "raw_query", "where_query"); query})
  class(where_query) <- c("list", "where_expr")

  if(inherits(query_list, "xafty_query")) {
    query_list$query <- append(query_list$query, raw_query)
    query_list$query[[length(query_list$query) + 1]] <- where_query
  } else {
    query_list <- append(query_list, raw_query)
    query_list[[length(query_list) + 1]] <- where_query
  }
  class(query_list) <- c("list", "xafty_query_list")
  query_list
}

create_where_filter <- function(expr) {
  filter_func <- function(data) {
    data[with(data, eval(expr)), , drop = FALSE]
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
