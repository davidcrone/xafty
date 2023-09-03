
#' @title Checks for Empty Entries
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @export
check_column_notempty <- function(check_table, validity_table) {

  xafty_data_types <- "##!!notempty"

  check_for_syntax <- as.data.frame(sapply(validity_table, grepl, pattern = xafty_data_types))

  if (!any(check_for_syntax)) {
    result <- FALSE
    message <- "Warning: Checked for not empty, but no entry with '##!!notempty' in validity table!"
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message))
  }

  not_empty_columns <- sapply(check_for_syntax, any)
  not_empty_columns <- names(not_empty_columns)[not_empty_columns]

  list_result <- list()

  for (i in not_empty_columns) {

    if (i %in% colnames(check_table)) {

      list_result[[i]] <- all(!is.na(check_table[[i]]))

    }

  }

  results_unlisted <- unlist(list_result)

  if (all(results_unlisted)) {

    result <- TRUE
    message <- paste0("ALL GOOD!")
    columns <- NA

    } else {

      result <- FALSE
      wrong_columns <- names(results_unlisted)[results_unlisted]
      wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
      columns <- wrong_columns_collapsed
      message <- paste0("Rule Broken: Column Not Empty. Following columns '##!!notempty' have NA entries: ")

  }

  data.frame("Check" = "Column Not Empty", "Check_Result" = result, "Message" = message, "Columns" = columns)

}
