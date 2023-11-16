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

  stopifnot((check_type %in% c("presence", "order")))

  colnames_check_table <- colnames(check_table)
  colnames_validity_table <- colnames(validity_table)

  logical_vector_no_order <- colnames_validity_table %in% colnames_check_table

  all_present <- all(logical_vector_no_order)

  if (!all_present) {
    if (simply) {
      return(FALSE)
    } else {
      result <- FALSE
      missing_column_names <- colnames_validity_table[!logical_vector_no_order]
      missing_column_names <- paste(missing_column_names, collapse = ", ")
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

#' @title Filter for Column Names
#' @description
#' The function filters for column names in two ways:
#' * Column names in the check table that are expected by the validity table
#' * Expected columns by the validity table that are not in the check table.
#'
#' This function also tries to implement column names that are stored in the validity table as regular expressions.
#' @param check_table Data Frame. The table that will be checked against the information in the validity table
#' @param validity_table Data Frame. A table that stores the column names in the first row
filter_column_names <- function(check_table, validity_table) {

  colnames_check_table <- colnames(check_table)
  colnames_validity_table <- colnames(validity_table)

  columns_in_validity_table <- !(colnames_validity_table %in% colnames_check_table)
  columns_in_check_table <- !(colnames_check_table %in% colnames_validity_table)


  list("colnames_validity_table" = columns_in_validity_table,
       "colnames_check_table" = columns_in_check_table)

}


#' @title Adds Columns to Validity Matched by Regular Expressions
#' @description
#' The rule ##!!regexcolumns specifies a regular expression as values below the rule that
#' represents all columns in the check table which match the regular expression. This function
#' adds these matched columns to the validity.
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A validation table that stores the rules that the check table will be checked against.
#' @param multiple Character.
#' The following Parameters are allowed:
#' \itemize{
#'  \item "remove": The default. Removes all columns that have been duplicated through multiple matches
#'  \item "keep": Keeps all columns that have been duplicated through multiple matches
#' }
#' @returns A Data Frame. The return value is the new validity table with column names added from the check table matched
#' by the regular expressions. Each column matched in this way also inherits the same set of rules specified
#' in the same column as the ##!!regexcolumns rule.
#' @export
add_regex_columns_to_validity <- function(check_table, validity_table, multiple = "remove") {

  stopifnot(multiple %in% c("remove", "keep"))

  xafty_syntax_regrex <- "##!!regexcolumns"

  xafty_pairs_regex <- obtain_columns_in_validity(validity_table = validity_table,
                                                  xafty_syntax = xafty_syntax_regrex)

  if(any(is.na(xafty_pairs_regex))) return(validity_table)

  columns_check_table <- colnames(check_table)

  n_pairs <- length(xafty_pairs_regex)

  list_help <- list()

  for (i in seq(n_pairs)) {

    xafty_pair <- xafty_pairs_regex[i]

    regex <- obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair)


    if(length(regex) <= 0) {
      warning(paste0("Expected a regular expression below rule ##!!regexcolumns, but no regular expression found.
                  The warning occured for the validity column: ", xafty_pair))
      next
    }

    regex_columns <- lapply(regex, \(x) columns_check_table[grepl(x, columns_check_table)])

    test_for_no_match <- all(sapply(regex_columns, \(x) length(x) <= 0))

    if(test_for_no_match) {
      warning(paste0("No columns matched with regular expression provided in validity column: ", xafty_pair))
      next
    }

    regex_columns <- do.call(c, regex_columns)

    validity_table_values <- validity_table[, xafty_pair]

    new_validity_columns <- lapply(regex_columns,
                                   FUN = \(regex_col){
                                     df <- data.frame(tmp_colname = validity_table_values)
                                     setNames(df, regex_col)
                                   })



    list_help[[i]] <-  do.call(cbind, new_validity_columns)

  }

  if (length(list_help) <= 0) {
    validity_table_pruned <- validity_table[, !(colnames(validity_table) %in% xafty_pairs_regex), drop = FALSE]
    return(validity_table_pruned)
  }

  validity_add_columns <- do.call(cbind, list_help)

  validity_add_column_names <- colnames(validity_add_columns)
  length_validity_add <- length(validity_add_column_names)

  uniqued_validity_add <- unique(validity_add_column_names)
  uniqued_length_validity_add <- length(uniqued_validity_add)

  if(length_validity_add != uniqued_length_validity_add) {

    if (multiple == "remove") {

      warning("Multiple matching of the same column by two or more regular expressions;
            Warning! Duplicated columns have been removed!")

      validity_add_columns <- validity_add_columns[!duplicated(validity_add_column_names)]

    } else if (multiple == "keep") {
      message("Multiple matching of same column(s) detected. Keeping all!")
    }

  }
  validity_table_pruned <- validity_table[, !(colnames(validity_table) %in% xafty_pairs_regex), drop = FALSE]

  cbind(validity_table_pruned, validity_add_columns)

}
