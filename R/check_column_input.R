
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

    position_xafty_syntax <- which(check_for_syntax[[i]])
    # Getting all Values below xafty syntax
    strict_values_raw <- validity_table[[i]][seq((position_xafty_syntax + 1), length(validity_table[[i]]))]
    strict_values_clean <- strict_values_raw[!is.na(strict_values_raw) & !grepl(xafty_syntax, strict_values_raw)]

    # Accounting for non integer values. Check validity will always be read as integer.
    # Thus, the comparison should always hold.

    check_table[[i]] <- as.character(check_table[[i]])

    # Account for NA.
    check_table_na_removed <- check_table[[i]][!is.na(check_table[[i]])]

   list_result[[i]] <-  sum(check_table_na_removed %in% strict_values_clean) == length(check_table_na_removed)

  }

  results_unlisted <- unlist(list_result)

  if (any(!results_unlisted)) {
    result <- FALSE
    wrong_columns <- names(results_unlisted)[!results_unlisted]
    wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
    message <- paste0("Rule Broken: Column Strict Input. Following column's input differ from specification: ", wrong_columns_collapsed)
  } else {

    result <- TRUE
    message <- paste0("ALL GOOD!")

  }

  data.frame("Check" = "Column Strict Input", "Check_Result" = result, "Message" = message)


}

check_column_strict_patterninput <- function(check_table, validity_table, xafty_data_type = "##!!strictpattern") {

  ### TODO: Function must check for all pattern types in the validity and then check each column according to its pattern rule

  check_for_syntax <- as.data.frame(sapply(validity_table, grepl, pattern = xafty_data_type))

  if (!any(check_for_syntax)) {
    result <- FALSE
    message <- paste0("Warning: Checked for pattern, but no entry with 'pattern rule' in validity table!")
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message))
  }

  pattern_columns <- sapply(check_for_syntax, any)
  pattern_columns <- names(pattern_columns)[pattern_columns]

  list_result <- list()

  for (i in pattern_columns) {

    position_xafty_syntax <- which(check_for_syntax[[i]])
    # Getting all Values below xafty syntax
    pattern_values_raw <- validity_table[[i]][seq((position_xafty_syntax + 1), length(validity_table[[i]]))]
    pattern_values_clean <- pattern_values_raw[!is.na(pattern_values_raw) & !grepl(xafty_data_type, pattern_values_raw)]

    # Accounting for non integer values. Check validity will always be read as integer.
    # Thus, the comparison should always hold.

    check_table[[i]] <- as.character(check_table[[i]])

    # Account for NA.
    check_table_na_removed <- check_table[[i]][!is.na(check_table[[i]])]

    df_presence <- as.data.frame(sapply(pattern_values_clean, \(col_name){
         sapply(check_table_na_removed, \(values) {
                 data.frame(col_name = values)
               })
         }))

    presence_vector <- sapply(colnames(df_presence), \(pattern) {
        grepl(pattern, df_presence[[pattern]], fixed = TRUE)
    })


    switch (xafty_data_type,
           "##!!strictpattern" = list_result[[i]] <-  all(presence_vector),
           "##!!rowpattern" = list_result[[i]] <- all(apply(presence_vector, 1, \(row) any(row))),
           "##!!anypattern" = list_result[[i]] <-  any(presence_vector)
    )


  }

  results_unlisted <- unlist(list_result)

  if (all(results_unlisted)) {
    result <- TRUE
    message <- paste0("ALL GOOD!")

    } else {

      result <- FALSE
      wrong_columns <- names(results_unlisted)[!results_unlisted]
      wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
      message <- paste0("Rule Broken: Column Pattern Input. Following column's values do not contain the specified pattern: ", wrong_columns_collapsed)


  }

  data.frame("Check" = "Column Pattern", "Check_Result" = result, "Message" = message)


}
