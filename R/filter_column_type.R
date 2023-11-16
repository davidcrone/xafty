#' @title Filter a Column by its Data Type given the Validity Table
#' @param check_table Data Frame. The table that will be checked against the information in the validity table
#' @param validity_table Data Frame. A table that stores the column names in the first row
#' @param filter_column Character. The column which will be checked. The column must be present in both the check table
#' as well as the validity table
#' @param date_origin Character. The date from which numeric dates will be conversed into ISO-Date format
#' @param tryFormats Character vector. Date formats that should be use to try to convert to date
#' @param tz Character. Timezone for the POSIXct values. Default is "" which translates to UTC
#' @return A Boolean vector. TRUE when the value is in line with the specified data type; FALSE when it is not
#' @export
filter_column_type <- function(check_table, validity_table, filter_column,
                               date_origin = "1899-12-30",
                               tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d"),
                               tz = "") {
  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if (!(filter_column %in% colnames(validity_table))) stop("Column is not present in validity table")
  if (!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  check_column <- check_table[, filter_column, drop = TRUE]
  validity_column <- validity_table[, filter_column, drop = FALSE]

  xafty_data_type <- names(obtain_columns_in_validity(validity_table = validity_column, xafty_syntax = xafty_data_types))

  switch(xafty_data_type,
    "##!!factor" = xafty_column <- sapply(check_column, \(x) is.factor(x), USE.NAMES = FALSE),
    "##!!text" = xafty_column <- sapply(check_column, \(x) is.character(x), USE.NAMES = FALSE),
    "##!!date" = xafty_column <- is.Date_xafty(check_column,
      date_origin = date_origin,
      tryFormats = tryFormats
    ),
    "##!!number" = xafty_column <- is.numeric_xafty(check_column),
    "##!!datetime" = xafty_column <- is.POSIXct_xafty(check_column, tz = tz)
  )

  xafty_column
}

filter_column_type_xafty_list <- function(check_table, validity_table, filter_column, xafty_rule, xafty_values = NULL,
                               date_origin = "1899-12-30",
                               tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d"),
                               tz = "") {
  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if (!(filter_column %in% colnames(validity_table))) stop("Column is not present in validity table")
  if (!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  stopifnot(xafty_rule %in% xafty_data_types)

  check_column <- check_table[, filter_column, drop = TRUE]
  validity_column <- validity_table[, filter_column, drop = FALSE]

  switch(xafty_rule,
         "##!!factor" = xafty_column <- sapply(check_column, \(x) is.factor(x), USE.NAMES = FALSE),
         "##!!text" = xafty_column <- sapply(check_column, \(x) is.character(x), USE.NAMES = FALSE),
         "##!!date" = xafty_column <- is.Date_xafty(check_column,
                                                    date_origin = date_origin,
                                                    tryFormats = tryFormats
         ),
         "##!!number" = xafty_column <- is.numeric_xafty(check_column),
         "##!!datetime" = xafty_column <- is.POSIXct_xafty(check_column, tz = tz)
  )

  # Filter columns that break the rule!
  !(xafty_column)
}

