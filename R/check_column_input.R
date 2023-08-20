
check_column_strictinput <- function(check_table, validity_table) {

  xafty_syntax <- "##!!"
  possible_classes <- c("strictinput")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  check_for_syntax <- as.data.frame(sapply(validity_table, grepl, pattern = xafty_data_types))

  if (!any(check_for_syntax)) {
    result <- FALSE
    message <- paste0("Warning: Checked for strict input, but no entry with '", xafty_data_types, "' in validity table!")
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message))
  }

  strict_input_columns <- sapply(check_for_syntax, any)
  strict_input_columns <- names(strict_input_columns)[strict_input_columns]

  list_result <- list()

  for (i in strict_input_columns) {



  }

  results_unlisted <- unlist(list_result)

  if (any(results_unlisted)) {
    result <- FALSE
    wrong_columns <- names(results_unlisted)[results_unlisted]
    wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
    message <- paste0("Rule Broken: Column Not Empty. Following columns have NA entries: ", wrong_columns_collapsed)
  } else {

    result <- TRUE
    message <- paste0("ALL GOOD!")

  }

  data.frame("Check" = "Column Not Empty", "Check_Result" = result, "Message" = message)


}
