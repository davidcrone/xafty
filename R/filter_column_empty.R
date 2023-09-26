

filter_column_empty <- function(check_table, validity_table, filter_column) {

  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if(!(filter_column %in% colnames(validity_table))) stop("Column is not present in validity table")
  if(!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_data_types <- "##!!notempty"

  check_column <- check_table[, filter_column, drop = TRUE]

  is.na(check_column)

}
