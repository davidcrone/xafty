
#' @title Checks for Empty Entries
#'
#' @export
check_column_notempty <- function(check_table, validity_table) {

  xafty_syntax <- "##!!"
  possible_classes <- c("notempty")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

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

      list_result[[i]] <- any(is.na(check_table[[i]]))

    }

  }

  results_unlisted <- unlist(list_result)

  if (any(results_unlisted)) {
    result <- FALSE
    wrong_columns <- names(results_unlisted)[results_unlisted]
    wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
    message <- paste0("Rule Broken: Column Not Empty. Following columns '##!!notempty' have NA entries: ", wrong_columns_collapsed)
  } else {

    result <- TRUE
    message <- paste0("ALL GOOD!")

  }

  data.frame("Check" = "Column Not Empty", "Check_Result" = result, "Message" = message)

}
