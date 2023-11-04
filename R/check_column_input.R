
#' @title Check a Table for Exact Value Rules
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
check_column_exactinput <- function(check_table, validity_table) {

  xafty_syntax <- "##!!"
  possible_checks <- c("anyexact", "strictexact", "eachexact")
  xafty_data_types <- paste0(xafty_syntax, possible_checks)

  columns_with_syntax <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_data_types)

  if (any(is.na(columns_with_syntax))) {
    result <- FALSE
    message <- paste0("Warning: Checked for exact input, but no entry with '", paste0(xafty_data_types, collapse = ", "),
                      "' in validity table!")
    columns <- NA
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message, "columns" = columns))
  }

  list_result <- list()

  for (i in seq(length(columns_with_syntax))) {

    syntax <- columns_with_syntax[i]

    exact_values <- obtain_values_in_validity(validity_table, xafty_pair = syntax)

    check_table[[syntax]] <- as.character(check_table[[syntax]])

    # Account for NA.
    check_table_na_removed <- check_table[[syntax]][!is.na(check_table[[syntax]])]

    switch (names(syntax),
            "##!!strictexact" = list_result[[syntax]] <-  sum(check_table_na_removed %in% exact_values) == length(check_table_na_removed),
            "##!!anyexact" = list_result[[syntax]] <- any(check_table_na_removed %in% exact_values),
            "##!!eachexact" = list_result[[syntax]] <- all(exact_values %in% check_table_na_removed)
    )

  }

  results_unlisted <- unlist(list_result)

  check_result <- all(results_unlisted)

  if (check_result) {

    result <- TRUE
    message <- paste0("ALL GOOD!")
    columns <- NA

    } else {

     result <- FALSE
     wrong_columns <- names(results_unlisted)[!results_unlisted]
     wrong_columns_collapsed <- paste0(wrong_columns, collapse = ", ")
     columns <- wrong_columns_collapsed
     message <- paste0("Rule Broken: Column Exact Input. Following column's input differ from specification: ")

  }

  data.frame("Check" = "Column Exact Input", "Check_Result" = result, "Message" = message, "columns" = columns)

}

#' @title Check a Table for Pattern Value Rules
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
check_column_patterninput <- function(check_table, validity_table) {

  xafty_syntax <- "##!!"
  possible_checks <- c("strictpattern", "rowpattern", "anypattern", "eachpattern")
  xafty_data_types <- paste0(xafty_syntax, possible_checks)

  columns_with_syntax <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_data_types)

  if (any(is.na(columns_with_syntax))) {
    result <- FALSE
    message <- paste0("Warning: Checked for pattern, but no entry with 'pattern rule' in validity table!")
    return(data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message))
  }

  list_result <- list()

  for (i in seq(length(columns_with_syntax))) {

    syntax <- columns_with_syntax[i]

    pattern_values <- obtain_values_in_validity(validity_table = validity_table, xafty_pair = syntax)

    # Accounting for non integer values. Check validity will always be read as integer.
    # Thus, the comparison should always hold.

    check_table[[syntax]] <- as.character(check_table[[syntax]])

    # Account for NA.
    check_table_na_removed <- check_table[[syntax]][!is.na(check_table[[syntax]])]

    df_presence <- as.data.frame(sapply(pattern_values, \(col_name){
         sapply(check_table_na_removed, \(values) {
                 data.frame(col_name = values)
               })
         }))

    presence_vector <- sapply(colnames(df_presence), \(pattern) {
        grepl(pattern, df_presence[[pattern]], fixed = TRUE)
    })

    switch (names(syntax),
           "##!!strictpattern" = list_result[[syntax]] <-  all(presence_vector),
           "##!!rowpattern" = list_result[[syntax]] <- all(apply(presence_vector, 1, \(row) any(row))),
           "##!!anypattern" = list_result[[syntax]] <-  any(presence_vector),
           "##!!eachpattern" = list_result[[syntax]] <- all(apply(presence_vector, 2, \(col) any(col)))
    )

  }

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
      message <- paste0("Rule Broken: Column Pattern Input. Following column's values do not contain the specified pattern: ")

  }

  data.frame("Check" = "Column Pattern", "Check_Result" = result, "Message" = message, "columns" = columns)

}
