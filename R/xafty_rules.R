#' Table of All Xafty Rules
#'
#' A simple table containing the xafty rules, their type and a description (soon)
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{syntax}{A character vector with xafty rules.}
#'   \item{type}{A character vector of rule types.}
#'   \item{description}{A character vector of rule description in prosa.}
#'   \item{check_function}{A list of functions for check purposes.}
#' }
#'
#' @source The data is curated by xafty developers and grows as new rules are added.
xafty_rules_table <- data.frame(
  "syntax" = c(
    "##!!notempty",
    "##!!anyexact",
    "##!!strictexact",
    "##!!eachexact",
    "##!!text",
    "##!!date",
    "##!!number",
    "##!!factor",
    "##!!datetime",
    "##!!strictpattern",
    "##!!rowpattern",
    "##!!anypattern",
    "##!!eachpattern",
    "##!!unique",
    "##!!regexcolumns"
  ),
  "type" = c(
    "value",
    "value",
    "value",
    "value",
    "data_type",
    "data_type",
    "data_type",
    "data_type",
    "data_type",
    "value",
    "value",
    "value",
    "value",
    "special",
    "special"
  ),
  "description" = c(
    "The column cannot have empty values.",
    "At least one value in the check table must match any of the provided values in the validity table",
    "All values in the check table must match the provided values in the validity table",
    "All provided values in the validity table must be matched at least once in the check table",
    "Column must be of data type character",
    "Column must be of data type Date",
    "Column must be of data type numeric",
    "Column must be of data type factor",
    "Column must be of data type POSIXct",
    "All values in the column must match to every pattern provided in the validity table",
    "All values of the column must match at least to one pattern in the validity table",
    "At least one value in the column must match to any pattern provided in the validity table",
    "Every pattern provided in the validity table must match at least once to any value in the check table",
    "No value in the column should be present more than once",
    "Applies all rules of the same column in the validity table to every column in the check table
    which have been matched by the provided regular expressions"
  ),
  "check_function" = I(list(
    check_column_notempty,
    check_column_exactinput,
    check_column_exactinput,
    check_column_exactinput,
    check_column_types,
    check_column_types,
    check_column_types,
    check_column_types,
    check_column_types,
    check_column_patterninput,
    check_column_patterninput,
    check_column_patterninput,
    check_column_patterninput,
    check_column_unique,
    check_column_names
  ))
)
