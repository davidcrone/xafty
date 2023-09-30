
#' @title Checks for Empty Entries
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @export
check_column_notempty <- function(check_table, validity_table) {

  xafty_notempty <- "##!!notempty"
  columns_with_syntax <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_notempty)

  if (any(is.na(columns_with_syntax))) {
    result <- FALSE
    message <- "Warning: Checked for not empty, but no entry with '##!!notempty' in validity table!"
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message))
  }

  list_result <- list()

  for (i in seq(length(columns_with_syntax))) {

    column_name <- columns_with_syntax[i]

    list_result[[i]] <- all(!filter_column_empty(check_table = check_table, filter_column = column_name))

  }

  names(list_result) <- columns_with_syntax

  results_unlisted <- unlist(list_result)

  if (all(results_unlisted)) {

    result <- TRUE
    message <- paste0("ALL GOOD!")
    columns <- NA

    } else {

      result <- FALSE
      wrong_columns <- names(results_unlisted)[!results_unlisted]
      wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
      columns <- wrong_columns_collapsed
      message <- paste0("Rule Broken: Column Not Empty. Following columns '##!!notempty' have NA entries: ")

  }

  data.frame("Check" = "Column Not Empty", "Check_Result" = result, "Message" = message, "Columns" = columns)

}
