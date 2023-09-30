
#' @title Filter Values in Column That Break Exact Input Rule
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @param filter_column Character. The column which will be checked. The column must be present in both the check table
#' as well as the validity table
#' @export
filter_column_exactinput <- function(check_table, validity_table, filter_column) {

  stopifnot(length(filter_column) == 1 & is.character(filter_column))

  if(!(filter_column %in% colnames(validity_table))) stop("Column is not present in validity table")
  if(!(filter_column %in% colnames(check_table))) stop("Column is not present in check table")

  xafty_syntax <- "##!!"
  possible_checks <- c("anyexact", "strictexact", "eachexact")
  xafty_data_types <- paste0(xafty_syntax, possible_checks)

  check_column <- check_table[, filter_column, drop = TRUE]
  validity_column <- validity_table[, filter_column, drop = FALSE]

  result_out <- rep(TRUE, length(check_column))

  xafty_pairs <- obtain_columns_in_validity(validity_table = validity_column, xafty_syntax = xafty_data_types)

  list_xafty_values <- list()

  for (i in seq(length(xafty_pairs))) {

    xafty_pair <- xafty_pairs[i]

    list_xafty_values[[i]] <- obtain_values_in_validity(validity_table = validity_column, xafty_pair = xafty_pair)

  }

  xafty_values <- do.call(c, list_xafty_values)

  position_na <- which(is.na(check_column))

  position_broken_exact <- which(!(check_column %in% xafty_values))

  position_broken_rule <- setdiff(position_broken_exact, position_na)

  if (length(position_broken_rule) <= 0) return(result_out)

  result_out[position_broken_rule] <- FALSE

  result_out

}
