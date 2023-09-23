
#' @title Filter a Column by its Data Type given the Validity
#' @param check_table Data Frame. The table that will be checked against the information in the validity table
#' @param validity_table Data Frame. A table that stores the column names in the first row
#' @param filter_column Character. The column which will be checked. The column must be present in both the check table
#' as well as the validity table
#' @param date_origin Character. The date from which numeric dates will be conversed into ISO-Date format
#' @return A Boolean vector. TRUE when the value is in line with the specified data type; FALSE when it is not
filter_column_type <- function(check_table, validity_table, filter_column, date_origin = "1899-12-30") {

  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if(!(filter_column %in% colnames(validity_table))) stop("Column is not present in validity table")
  if(!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  check_column <- check_table[, filter_column, drop = TRUE]
  validity_column <- validity_table[, filter_column, drop = FALSE]

  xafty_data_type <- names(obtain_columns_in_validity(validity_table = validity_column, xafty_syntax = xafty_data_types))


  switch (xafty_data_type,
    "##!!factor" = xafty_column <- sapply(check_column, \(x) is.factor(x), USE.NAMES = FALSE),
    "##!!text" = xafty_column <- sapply(check_column, \(x) is.character(x), USE.NAMES = FALSE),
    "##!!date" = xafty_column <- sapply(check_column, \(x) is.Date_xafty(x, date_origin = date_origin), USE.NAMES = FALSE),
    "##!!number" = xafty_column <- sapply(check_column, \(x) is.numeric_xafty(x), USE.NAMES = FALSE),
    "##!!datetime" = xafty_column <- sapply(check_column, \(x) is.POSIXct_xafty(x), USE.NAMES = FALSE)
  )

  xafty_column

}
