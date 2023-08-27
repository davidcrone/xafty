
#' @title Main Function to Check a Table for its Validity
#'
#' @description
#' The function checks a table (the check table) for its validity using a table (the validity table)
#' that is crafted with xafty rules to specify the expected column names and values.
#'
#'
#' @param check_table Data Frame. The table that will be checked against the information in the validity table
#' Important: You will need to make sure that empty values are correctly identified with NA.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @param col_number Boolean. Should the count of columns be checked?
#' @param col_names Character. Following parameters are allowed:
#' \itemize{
#'  \item "presence": If the simple presence of each column suffices
#'  \item "order": If the columns should be present as well as in the specified order.
#'  \item FALSE: If column names should not be checked
#' }
#' @param col_classes Boolean. Should the column's classes be checked?
#' @param values_notempty Boolean. Should columns be checked for NA entries?
#' @param values_exact Boolean. Should columns be checked for exact value rules?
#' @param values_pattern Boolean. Should columns be checked for pattern value rules?
#'
#' @returns Data Frame. A table that stores the check results.
#' @export
check_validity <- function(check_table, validity_table,
                           column_number = TRUE,
                           column_names = "presence",
                           column_classes = TRUE,
                           values_notempty = TRUE,
                           values_exact = TRUE,
                           values_pattern = "##!!strictpattern") {

  df_result_out <- create_result_df()

  if(column_number) {
    df_result_out[df_result_out$Check == "Column Number", ] <- check_column_number(check_table, validity_table)
  }
# TODO: Parameter check_order is not working yet
  if(!isFALSE(column_names)) {
    df_result_out[df_result_out$Check == "Column Names", ] <- check_column_names(check_table, validity_table, check_type = column_names)
  }

  if(column_classes) {
    df_result_out[df_result_out$Check == "Column Classes", ] <- check_column_classes(check_table, validity_table)
  }

  if(values_notempty) {
    df_result_out[df_result_out$Check == "Values Notempty", ] <- check_column_notempty(check_table, validity_table)
  }

  if(values_exact) {
    df_result_out[df_result_out$Check == "Values Exact", ] <- check_column_strictinput(check_table, validity_table)
  }

  if(!isFALSE(values_pattern)) {
    df_result_out[df_result_out$Check == "Values Pattern", ] <- check_column_strict_patterninput(check_table, validity_table, xafty_data_type = values_pattern)

  }

  df_result_out
}
