
#' @title Main Function to Check a Table for its Validity
#'
#' @description
#' The function checks a table (the check table) for its validity using a table (the validity table)
#' that contains the xafty rules to specify the expected column names and values.
#'
#' \strong{!! Important:} You will need to make sure that empty values are correctly identified as NA.
#'
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A validation table that stores the rules that the check table will be checked against.
#' @param column_number Boolean. Should the count of columns be checked?
#' @param column_names Character. Following parameters are allowed:
#' \itemize{
#'  \item "presence": If the simple presence of each column suffices
#'  \item "order": If the columns should be present as well as in the specified order
#'  \item FALSE: If column names should not be checked
#' }
#' @param column_types Boolean. Should the column's data types be checked?
#' @param values_notempty Boolean. Should columns be checked for NA entries?
#' @param values_exact Boolean. Should columns be checked for exact value rules?
#' @param values_pattern Boolean. Should columns be checked for pattern value rules?
#'
#' @returns A data frame that stores the check results.
#' The data frame will contain the following columns:
#' \itemize{
#'  \item Check: Contains the names of the rule
#'  \item Check_Result: Boolean values representing the result of the check. Check passed: TRUE, check failed: FALSE
#'  \item Message: Detailed information about the check result
#'  \item Columns: Name of the column(s) where the respective rule was broken
#' }
#'
#' @export
check_validity <- function(check_table, validity_table,
                           column_number = TRUE,
                           column_names = "presence",
                           column_types = TRUE,
                           values_notempty = TRUE,
                           values_exact = TRUE,
                           values_pattern = TRUE) {

  df_result_out <- create_result_table()

  if(column_number) {
    df_result_out[df_result_out$Check == "Column Number", ] <- check_column_number(check_table, validity_table)
  }

  if(!isFALSE(column_names)) {
    df_result_out[df_result_out$Check == "Column Names", ] <- check_column_names(check_table, validity_table, check_type = column_names)
  }

  if(column_types) {
    df_result_out[df_result_out$Check == "Column Types", ] <- check_column_types(check_table, validity_table)
  }

  if(values_notempty) {
    df_result_out[df_result_out$Check == "Values Notempty", ] <- check_column_notempty(check_table, validity_table)
  }

  if(values_exact) {
    df_result_out[df_result_out$Check == "Values Exact", ] <- check_column_exactinput(check_table, validity_table)
  }

  if(values_pattern) {
    df_result_out[df_result_out$Check == "Values Pattern", ] <- check_column_patterninput(check_table, validity_table)

  }

  df_result_out

}
