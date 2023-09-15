
#' @title Reads in Example Data for Test Purposes
#'
#' @param path_check Path to the table that will be checked
#' @param path_validity Path to the validity table that stores the accompanying xafty rules
#' @param file_ending Character. Valid input: "xlsx", "csv_comma", "csv_semicolon". You need to make sure that the file ending
#' matches the actual file.

read_check_and_validity <- function(path_check = "inst/extdata/example_data.xlsx",
                                    path_validity = "inst/extdata/example_validity.csv", file_ending = "xlsx") {

  validity_table <- read.csv(path_validity, na.strings = "")

  check_table <- switch(file_ending,
                        "xlsx" = readxl::read_xlsx(path_check, col_types = "text"),
                        "csv_comma" = read.csv(path_check, colClasses = "character"),
                        "csv_semicolon" = read.csv2(path_check, colClasses = "character")
  )

  list(
    validity_table = validity_table,
    check_table = check_table
  )

}

#' @title Test Core Function of check_validity()
test_main_functions <- function() {

  data <- read_check_and_validity()
  data_check_aligned <- align_column_classes(data$check_table, data$validity_table)
  check_validity(data_check_aligned, data$validity_table)

}


#' @title Template of Result Data Frame for check_validity()
create_result_table <- function() {

  arg_names <- names(formals(check_validity))[!(names(formals(check_validity)) %in% c("check_table", "validity_table"))]

  list_out <- list()

  for(arg_name in arg_names) {
    # Create the result variable name
    result_var_name <- paste0("result_", arg_name)

    # Create the friendly name for the check
    check_name <- gsub("_", " ", arg_name)
    check_name <- tools::toTitleCase(check_name)

    # Assign the result row to the dynamically created variable name
    list_out[[arg_name]] <- assign(result_var_name, create_result_row(check_name))
  }

  df_result_out <- do.call(rbind, list_out)
  rownames(df_result_out) <- NULL

  df_result_out

}


#' @title Row Template for Result Data Frame
#' @param check_name Character string specifying the name of the check to be performed.
#' @param default_result Logical value representing the default result of the check. Default is \code{TRUE}.
#' @param default_message Character string representing the default message for the check result. Default is \code{"NOT CHECKED!"}.
#' @param default_columns Default columns to be included in the result data frame. Default is \code{NA}.
create_result_row <- function(check_name, default_result = TRUE, default_message = "NOT CHECKED!", default_columns = NA) {
  data.frame("Check" = check_name, "Check_Result" = default_result, "Message" = default_message, "Columns" = default_columns)
}

#' @title Checks Validity Table for Presence of Xafty Syntax
#' @param validity_table A validity table that will be checked for xafty rules
#' @param xafty_syntax The xafty rules which presence will be checked in the validity table
#' @return Returns a named vector that has the column name as value and the rule as the name. This named vector is also
#' referred to as "xafty pair".
obtain_columns_in_validity <- function(validity_table, xafty_syntax) {

  presence_list <- sapply(xafty_syntax, \(syntax){
    sapply(validity_table, grepl, pattern = syntax, fixed = TRUE)
  }, simplify = FALSE)

  presence_list <- lapply(presence_list, \(item){
      presence_table <- as.data.frame(item)
      sapply(presence_table, any)
  })

  list_out <- list()

  for (i in seq(length(xafty_syntax))) {

    syntax <- xafty_syntax[i]
    presence_vector <- presence_list[[syntax]]
    presence_columns <- names(presence_vector)[presence_vector]

    list_out[[i]] <- setNames(object = presence_columns, rep(syntax, length(presence_columns)))

  }

 result <- do.call(c, list_out)

 if(length(result) <= 0) {

   result <- NA
   warning("No matching xafty_syntax in validity table. Returning NA")

   }

 result

}

#' @title Get Corresponding Values for Xafty Rules
#' @param validity_table A validity table that will be checked for xafty rules
#' @param xafty_pair A named vector of length 1 that is named with the xafty rule and has the value of a column name
obtain_values_in_validity <- function(validity_table, xafty_pair) {

    xafty_rule <- names(xafty_pair)
    values_column <- validity_table[, xafty_pair]
    rule_position <- which(values_column == xafty_rule)

    values_below_rule <- values_column[seq(rule_position + 1, length(values_column))]

    if(any(grepl("##!!", values_below_rule))) {

      first_xafty_syntax <- min(which(grepl("##!!", values_below_rule)))
      values_between <- values_below_rule[seq(first_xafty_syntax - 1)]
      values_return <- values_between[!is.na(values_between)]

      if(length(values_return) <= 0) stop(paste("No values found for", names(xafty_pair)))

    } else {

      values_return <- values_below_rule[!is.na(values_below_rule)]

    }

    values_return
}
