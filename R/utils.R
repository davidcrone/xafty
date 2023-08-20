
#' @param file_ending Character. Valid input: "xlsx", "csv_comma", "csv_semicolon". You need to make sure that the file ending
#' matches the actual file.

read_check_and_validity <- function(path_check, path_validity, file_ending = "xlsx") {

  validity_table <- read.csv(path_validity, na.strings = "NA")

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
