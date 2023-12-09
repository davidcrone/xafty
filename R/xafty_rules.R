#' Table of All Xafty Rules
#'
#' A simple table containing the xafty rules, their type, a description, and useful functions
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{syntax}{A character vector with xafty rules.}
#'   \item{type}{A character vector of rule types.}
#'   \item{description}{A character vector of rule description in prosa.}
#'   \item{check_function}{A list of functions for check purposes.}
#'   \item{filter_function}{A list of functions to filter values.}
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
  )),
  "filter_function" = I(list(
    filter_column_empty_xafty_list,
    filter_column_exactinput_xafty_list,
    filter_column_exactinput_xafty_list,
    filter_column_exactinput_xafty_list,
    filter_column_type_xafty_list,
    filter_column_type_xafty_list,
    filter_column_type_xafty_list,
    filter_column_type_xafty_list,
    filter_column_type_xafty_list,
    filter_column_patterninput_xafty_list,
    filter_column_patterninput_xafty_list,
    filter_column_patterninput_xafty_list,
    filter_column_patterninput_xafty_list,
    NULL,
    NULL
  )),
  "change_type_function" = I(list(
    NULL,
    NULL,
    NULL,
    NULL,
    as.character,
    as.Date_xafty,
    as.numeric,
    as.factor,
    as.POSIXct_xafty,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  ))
)


