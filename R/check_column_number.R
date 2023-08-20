
#' @title Check for Correct Number of Columns
#'

check_column_number <- function(check_table, validity_table) {

  n_cols_check_table <- ncol(check_table)
  n_cols_validity_table <- ncol(validity_table)
  result <- n_cols_check_table == n_cols_validity_table

  if(!result) {
    message <- paste("Rule Broken: Wrong number of columns! Table should have", n_cols_validity_table, "columns but has",
                         n_cols_check_table, "columns.", sep = " ")
  } else {
    message <- paste("ALL GOOD!")
  }

  data.frame("Check" = "Column Number", "Check_Result" = result, "Message" = message)

}
