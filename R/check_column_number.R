
#' @title Check for Correct Number of Columns
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @param check_type Character.
#' Following Parameters are allowed:
#' \itemize{
#'  \item "equal": If number of columns in both tables should be equal
#'  \item "larger": If number of columns in both tables should be equal or larger
#' }
#' @export
check_column_number <- function(check_table, validity_table, check_type = "equal") {

  n_cols_check_table <- ncol(check_table)
  n_cols_validity_table <- ncol(validity_table)

  if(check_type == "larger") {

    result <- n_cols_check_table >= n_cols_validity_table

  } else {

    result <- n_cols_check_table == n_cols_validity_table

  }


  if(result) {

    message <- paste("ALL GOOD!")

  } else {

    message <- paste("Rule Broken: Wrong number of columns! Table should have", n_cols_validity_table, "column(s) but has",
                     n_cols_check_table, "columns.", sep = " ")

  }

  data.frame("Check" = "Column Number", "Check_Result" = result, "Message" = message, "Columns" = NA)

}
