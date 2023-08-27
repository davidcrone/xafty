
#' @title Check for Column Names
#'
#' @export
check_column_names <- function(check_table, validity_table, check_type = "presence") {

  colnames_check_table <- colnames(check_table)
  colnames_validity_table <- colnames(validity_table)

  logical_vector_no_order <- colnames_validity_table %in% colnames_check_table

  all_present <- all(logical_vector_no_order)

  if(!all_present) {

    missing_column_names <- colnames_validity_table[!logical_vector_no_order]
    missing_column_names <- paste(missing_column_names, collapse = ", ")
    result <-  FALSE
    message <- paste("Rule Broken: Column names. Following columns are missing:")
    columns <- missing_column_names

    return(data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message, "Columns" = columns))

  }

  if (check_type == "presence") {

    result <-  TRUE
    message <- paste("ALL GOOD!")
    columns <- NA

  }

  if(check_type == "order") {

    first_column <- colnames_validity_table[1]
    last_column <- colnames_validity_table[length(colnames_validity_table)]

    position_check_first <- which(colnames_check_table %in% first_column)
    position_check_last <- which(colnames_check_table %in% last_column)

    check_columns_between <- colnames_check_table[seq(position_check_first, position_check_last)]

    result_order <- all(colnames_validity_table == check_columns_between)

    if (result_order) {

      result <-  TRUE
      message <- "ALL GOOD!"
      columns <- NA

    } else {

      result <-  FALSE
      message <- "All columns are present but not in the specified order as given in the validity table"
      columns <- NA

    }

  }

  data.frame("Check" = "Column Names", "Check_Result" = result, "Message" = message, "Columns" = columns)

}