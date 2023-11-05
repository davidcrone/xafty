#' @title Check for Column Names
#'
#' @description
#' Checks a table if all columns by name are present.
#'
#' @param check_table Data Frame. The table that will be checked against the information in the validity table
#' @param validity_table Data Frame. A table that stores the column names in the first row
#' @param check_type Character.
#' Following parameters are allowed:
#' \itemize{
#'  \item "presence": If the simple presence of each column suffices
#'  \item "order": If the columns should be present as well as in the specified order
#' }
#' @param simply Boolean. Changes the return value of the function to a single logical vector of length 1.
#' @return A data.frame if simply is FALSE and a Boolean of length 1 if simply is TRUE
#' @export
check_column_names <- function(check_table, validity_table, check_type = "presence", simply = FALSE) {
  colnames_check_table <- colnames(check_table)
  colnames_validity_table <- colnames(validity_table)

  logical_vector_no_order <- colnames_validity_table %in% colnames_check_table

  all_present <- all(logical_vector_no_order)

  if (!all_present) {
    if (simply) {
      return(FALSE)
    } else {
      missing_column_names <- colnames_validity_table[!logical_vector_no_order]
      missing_column_names <- paste(missing_column_names, collapse = ", ")
      result <- FALSE
      message <- paste("Rule Broken: Column names. Following columns are missing:")
      columns <- missing_column_names

      return(data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message, "Columns" = columns))
    }
  }

  if (check_type == "presence") {
    result <- TRUE
    message <- paste("ALL GOOD!")
    columns <- NA
  }

  if (check_type == "order") {
    first_column <- colnames_validity_table[1]
    last_column <- colnames_validity_table[length(colnames_validity_table)]

    position_check_first <- which(colnames_check_table %in% first_column)
    position_check_last <- which(colnames_check_table %in% last_column)

    check_columns_between <- colnames_check_table[seq(position_check_first, position_check_last)]

    result_order <- all(suppressWarnings(colnames_validity_table == check_columns_between))

    if (result_order) {
      result <- TRUE
      message <- "ALL GOOD!"
      columns <- NA
    } else {
      result <- FALSE
      message <- "All columns are present but not in the specified order"
      columns <- NA
    }
  }

  if (simply) {
    return(result)
  } else {
    return(data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message, "Columns" = columns))
  }
}
