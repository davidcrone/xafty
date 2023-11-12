#' @title Filter Values in Column That are NA
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A validation table that stores the rules that the check table will be checked against.
#' @param filter_column Character. The column which will be checked. The column must be present in both the check table
#' as well as the validity table
#' @return A logical vector.
#' @export
filter_column_empty <- function(check_table, validity_table, filter_column) {
  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if (!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  na_values <- obtain_values_in_validity(validity_table = validity_table, xafty_pair = filter_column)
  na_values <- c(NA, na_values)

  check_column <- check_table[, filter_column, drop = TRUE]

  check_column %in% na_values
}
