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
#' @param column_number Character or Boolean. Should the count of columns be checked?
#' Following values are allowed:
#' \itemize{
#'  \item "equal": If number of columns in both tables should be equal
#'  \item "larger": If number of columns in both tables should be equal or larger
#'  \item FALSE: If column number should not be checked
#' }
#' @param column_names Character or Boolean. Should the column names be checked
#' Following values are allowed:
#' \itemize{
#'  \item "presence": If the simple presence of each column suffices
#'  \item "order": If the columns should be present as well as in the specified order
#'  \item FALSE: If column names should not be checked
#' }
#' @param column_types Boolean. Should the column's data types be checked?
#' @param values_notempty Boolean. Should columns be checked for NA entries or values representing NA values?
#' @param values_exact Boolean. Should columns be checked for exact value rules?
#' @param values_pattern Boolean. Should columns be checked for pattern value rules?
#' @param multiple_regex_columns Character. Whether to keep duplicated columns that have been added via
#' the rule ##!!regexcolumns or remove them.
#' The following values are allowed:
#' \itemize{
#'  \item "remove": Removes duplicated columns.
#'  \item "keep": Keeps all duplicated columns.
#' }
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
                           column_number = "equal",
                           column_names = "presence",
                           column_types = TRUE,
                           values_notempty = TRUE,
                           values_exact = TRUE,
                           values_pattern = TRUE,
                           multiple_regex_columns = "remove") {

  validity_table <- add_regex_columns_to_validity(check_table = check_table,
                                                  validity_table = validity_table,
                                                  multiple = multiple_regex_columns)
  df_result_out <- create_result_table()

  if (!isFALSE(column_number)) {
    df_result_out[df_result_out$Check == "Column Number", ] <- check_column_number(check_table, validity_table,
      check_type = column_number
    )
  }

  if (!isFALSE(column_names)) {
    df_result_out[df_result_out$Check == "Column Names", ] <- check_column_names(check_table, validity_table,
      check_type = column_names
    )
  }

  if (column_types) {
    df_result_out[df_result_out$Check == "Column Types", ] <- check_column_types(check_table, validity_table)
  }

  if (values_notempty) {
    df_result_out[df_result_out$Check == "Values Notempty", ] <- check_column_notempty(check_table, validity_table)
  }

  if (values_exact) {
    df_result_out[df_result_out$Check == "Values Exact", ] <- check_column_exactinput(check_table, validity_table)
  }

  if (values_pattern) {
    df_result_out[df_result_out$Check == "Values Pattern", ] <- check_column_patterninput(check_table, validity_table)
  }

  df_result_out
}
