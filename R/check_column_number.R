
#' @title Check for Correct Number of Columns
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @export
check_column_number <- function(check_table, validity_table) {

  n_cols_check_table <- ncol(check_table)
  n_cols_validity_table <- ncol(validity_table)
  result <- n_cols_check_table == n_cols_validity_table

  if(result) {

    message <- paste("ALL GOOD!")

  } else {

    message <- paste("Rule Broken: Wrong number of columns! Table should have", n_cols_validity_table, "column(s) but has",
                     n_cols_check_table, "columns.", sep = " ")

  }

  data.frame("Check" = "Column Number", "Check_Result" = result, "Message" = message, "Columns" = NA)

}
