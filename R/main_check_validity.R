
#' @title Main Function to Check Validity of a Table
#'
#' @param check_table Character. A file path to the table will be checked for validity
#' @param validity_table Character. A file path to a special csv file that contains the valid syntax
#' @param check_number Boolean. Should the number of columns be checked?
#' @param check_name Boolean. Should the column names be checked?
#' @param check_classes Boolean. Should the column's classes be checked?
#' @param check_notempty Boolean. Should columns be checked for NA entries?
#'
#' @importFrom readxl read_xlsx
#' @export
check_validity <- function(check_table, validity_table,
                           check_number = TRUE,
                           check_names = TRUE,
                           check_classes = TRUE,
                           check_notempty = TRUE) {

  result <- TRUE
  message <- "NOT CHECKED!"

  result_check_number <- data.frame("Check" = "Column Number", "Check_Result" = result, "Message" = message)
  result_check_names <- data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message)
  result_check_classes <- data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message)
  result_check_notempty <- data.frame("Check" = "Column Not Empty", "Check_Result" = result, "Message" = message)

  if(check_number) {
    result_check_number <- check_column_number(check_table, validity_table)
  }

  if(check_names) {
    result_check_names <- check_column_names(check_table, validity_table, check_order = FALSE)
  }

  if(check_classes) {
    result_check_classes <- check_column_classes(check_table, validity_table)
  }

  if(check_notempty) {
    result_check_notempty <- check_column_notempty(check_table, validity_table)
  }

  check_results <- rbind(
    result_check_number,
    result_check_names,
    result_check_classes,
    result_check_notempty
    )

  check_results
}
