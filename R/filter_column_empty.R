
#' @title Filter Values in Column That are NA
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param filter_column Character. The column which will be checked. The column must be present in both the check table
#' as well as the validity table
#' @return A logical vector.
#' @export
filter_column_empty <- function(check_table, filter_column) {

  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if(!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_data_types <- "##!!notempty"

  check_column <- check_table[, filter_column, drop = TRUE]

  is.na(check_column)

}
