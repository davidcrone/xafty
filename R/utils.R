
#' @param file_ending Character. Valid input: "xlsx", "csv_comma", "csv_semicolon". You need to make sure that the file ending
#' matches the actual file.

read_check_and_validity <- function(path_check, path_validity, file_ending = "xlsx") {

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


test_main_functions <- function(path_check = "data/example_data.xlsx",
                                path_validity = "data/example_validity.csv") {

  data <- read_check_and_validity(path_check = path_check, path_validity = path_validity)
  data_check_aligned <- align_column_classes(data$check_table, data$validity_table)
  check_validity(data_check_aligned, data$validity_table)

}



create_result_df <- function() {

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

create_result_row <- function(check_name, default_result = TRUE, default_message = "NOT CHECKED!", default_columns = NA) {
  data.frame("Check" = check_name, "Check_Result" = default_result, "Message" = default_message, "Columns" = default_columns)
}
