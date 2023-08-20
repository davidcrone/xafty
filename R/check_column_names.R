
#' @title Check for Column Names
#'
#' @export
check_column_names <- function(check_table, validity_table, check_order = FALSE) {

  colnames_check_table <- colnames(check_table)
  colnames_validity_table <- colnames(validity_table)

  logical_vector_no_order <- colnames_validity_table %in% colnames_check_table

  if(all(logical_vector_no_order) & !check_order) {
    result <-  TRUE
    message <- paste("ALL GOOD!")

  } else {

    missing_column_names <- colnames_validity_table[!logical_vector_no_order]
    missing_column_names <- paste(missing_column_names, collapse = ", ")
    result <-  FALSE
    message <- paste("Rule Broken: Column names. Following columns are missing:", missing_column_names)
  }

# TODO: Add check for check_order = TRUE

  data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message)

}
