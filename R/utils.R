#' @title Reads in Example Data for Test Purposes
#'
#' @param path_check Path to the table that will be checked
#' @param path_validity Path to the validity table that stores the accompanying xafty rules
#' @param file_ending Character. Valid input: "xlsx", "csv_comma", "csv_semicolon". You need to make sure that the file ending
#' matches the actual file.
#' @importFrom readxl read_xlsx
#' @importFrom utils read.csv read.csv2
read_example_data <- function(path_check = "inst/extdata/example_data.xlsx",
                              path_validity = "inst/extdata/example_validity.csv", file_ending = "xlsx") {
  validity_table <- utils::read.csv(path_validity, na.strings = "")

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
  data <- read_example_data()
  data_check_aligned <- align_column_types(data$check_table, data$validity_table)
  check_validity(data_check_aligned, data$validity_table)
}


#' @title Template of Result Data Frame for check_validity()
create_result_table <- function() {

  ignored_args <- c("check_table", "validity_table", "multiple_regex_columns")
  arg_names <- names(formals(check_validity))[!(names(formals(check_validity)) %in% ignored_args)]

  list_out <- list()

  for (arg_name in arg_names) {
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
#'
#' @importFrom stats setNames
#' @return Returns a named vector that has the column name as value and the rule as the name. This named vector is also
#' referred to as "xafty pair".
obtain_columns_in_validity <- function(validity_table, xafty_syntax) {
  # If the number of rows of validity table are exactly 1 the behavior of the function is incorrect since sapply
  # creates a logical vector instead of a logical matrix if the validity table has only one row
  # Hope this doesn't introduce unforeseen consequences! :)
  if (nrow(validity_table) == 1) validity_table <- rbind(validity_table, validity_table)

  presence_list <- sapply(xafty_syntax, \(syntax){
    sapply(validity_table, \(column) column == syntax)
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

    list_out[[i]] <- stats::setNames(object = presence_columns, rep(syntax, length(presence_columns)))
  }


  result <- do.call(c, list_out)

  result <- result[!is.na(result)]

  if (length(result) <= 0) {
    result <- NA
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

  if (any(grepl("##!!", values_below_rule))) {
    first_xafty_syntax <- min(which(grepl("##!!", values_below_rule)))
    position_minus <- first_xafty_syntax - 1

    if (position_minus <= 0) {
      return(character(0))
    }

    values_between <- values_below_rule[seq(position_minus)]
    values_return <- values_between[!is.na(values_between)]

    if (length(values_return) <= 0) {
      return(character(0))
    }
  } else {
    values_return <- values_below_rule[!is.na(values_below_rule)]
  }

  values_return
}


#' @title Obtain the Column Names Separately
#' @param column_string Column names as a single string that have been separated by ", "
#' @return The column names in a vector
obtain_invalid_columns <- function(column_string) {
  strsplit(column_string, split = ", ")[[1]]
}

#' @title Check if Passed Values can be Parsed as Date
#' @param dates Character vector of Dates to be Parsed
#' @param date_origin Character. The date from which numeric dates will be conversed into ISO-Date format
#' @param tryFormats Character vector. Date formats that should be use to try to convert to date
#' @return An equally length boolean vector whether the value can be parsed as a Date given the specified formats and origin
#' @export
is.Date_xafty <- function(dates, date_origin = "1899-12-30", tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d")) {
  xafty_column <- sapply(dates, \(date) {
    tryCatch(
      {
        as.Date(date, tryFormats = tryFormats)

        TRUE
      },
      error = function(e) {
        numeric_date <- suppressWarnings(as.numeric(date))
        if (is.na(numeric_date)) {
          return(FALSE)
        }
        as.Date(numeric_date, origin = date_origin)

        TRUE
      }
    )
  })

  names(xafty_column) <- NULL

  xafty_column
}

#' @title Convert Values to Date Types
#' @param dates Character vector of Dates to be Converted
#' @param date_origin Character. The date from which numeric dates will be converted into ISO-Date format
#' @param tryFormats Character vector. Date formats that should be use to try to convert to date
#' @param force_type Boolean. Whether to force the type conversion even if it introduces NAs during type conversion. If TRUE
#' the function keeps the column as is.
#' @return An equally length date vector, \code{NA} when the value could not be converted to date
#' @export
as.Date_xafty <- function(dates, date_origin = "1899-12-30",
                          force_type = TRUE, tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d")) {

  if (force_type) {
    xafty_column <- sapply(dates, \(date) {
      tryCatch(
        {
          as.Date(date, tryFormats = tryFormats)
        },
        error = function(e) {
          numeric_date <- suppressWarnings(as.numeric(date))
          if (is.na(numeric_date)) {
            return(NA)
          }

          as.Date(numeric_date, origin = date_origin)
        }
      )
    })

    names(xafty_column) <- NULL

    as.Date(xafty_column, origin = "1970-01-01")

  } else {

    na_position <- which(is.na(dates))
    xafty_column <- sapply(dates, \(date) {
      tryCatch(
        {
          as.Date(date, tryFormats = tryFormats)
        },
        error = function(e) {
          numeric_date <- suppressWarnings(as.numeric(date))
          if (is.na(numeric_date)) {
            return(NA)
          }

          as.Date(numeric_date, origin = date_origin)
        }
      )
    })

    na_position_after <- which(is.na(xafty_column))

    if(sum(na_position_after) > sum(na_position)) {
      return(dates)
    } else {
      names(xafty_column) <- NULL
      as.Date(xafty_column, origin = "1970-01-01")
    }

  }


}

#' @title Check if Passed Values can be Parsed as Numeric
#' @param numbers Character vector of Numbers to be Parsed
#' @return An equally length Boolean vector whether the values can be parsed as numbers
#' @export
is.numeric_xafty <- function(numbers) {
  xafty_column <- rep(TRUE, length(numbers))

  position_na <- which(is.na(numbers))
  position_na_conversion <- which(is.na(suppressWarnings(as.numeric(numbers))))

  position_not_numbers <- setdiff(position_na_conversion, position_na)

  xafty_column[position_not_numbers] <- FALSE

  xafty_column
}

#' @title Check if Passed Values can be Parsed as Numeric
#' @param numbers Character vector of Numbers to be converted
#' @param force_type Boolean. Whether to force the type conversion even if it introduces NAs during type conversion. If TRUE
#' the function keeps the column as is.
#' @return An equally length numeric vector if the numerics could be successfully converted to NA.
#' @export
as.numeric_xafty <- function(numbers, force_type = TRUE) {

  position_na <- which(is.na(numbers))

  position_na_conversion <- which(is.na(suppressWarnings(as.numeric(numbers))))

  if(sum(position_na_conversion) > sum(position_na) & !force_type) {
    return(numbers)
  } else {
    as.numeric(numbers)
  }

}

#' @title Check if Passed Values can be Parsed as POSIXct
#' @param datetimes Character vector of date time values to be parsed
#' @param tz Timezone for the POSIXct values. Default is UTC
#' @return An equally length Boolean vector whether the values can be parsed as POSIXct
#' @export
is.POSIXct_xafty <- function(datetimes, tz = "") {
  xafty_column <- sapply(datetimes, \(datetime) {
    tryCatch(
      {
        as.POSIXct(datetime, tz = tz)

        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
  })

  names(xafty_column) <- NULL

  xafty_column
}

#' @title Convert Passed Values to POSIXct
#' @param datetimes Character vector of date time values to be parsed
#' @param tz Timezone for the POSIXct values. Default is UTC
#' @param force_type Boolean. Whether to force the type conversion even if it introduces NAs during type conversion. If TRUE
#' the function keeps the column as is.
#' @param tryFormats Character vector. POSIXct formats that should be use to try to convert to POSIXct
#' the function keeps the column as is
#' @export
as.POSIXct_xafty <- function(datetimes, force_type = TRUE,
                             tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                            "%Y/%m/%d %H:%M:%OS",
                                            "%Y-%m-%d %H:%M",
                                            "%Y/%m/%d %H:%M",
                                            "%Y-%m-%d",
                                            "%Y/%m/%d"),
                             tz = "") {

  if (force_type) {
    xafty_column <- sapply(datetimes, \(datetime) {
      tryCatch(
        {
          as.POSIXct(datetime, tz = tz)
        },
        error = function(e) {
          NA
        }
      )
    })

    names(xafty_column) <- NULL
    as.POSIXct(xafty_column, tryFormats = tryFormats)

  } else {
    na_position <- which(is.na(datetimes))

    xafty_column <- sapply(datetimes, \(datetime) {
      tryCatch(
        {
          as.POSIXct(datetime, tz = tz)
        },
        error = function(e) {
          NA
        }
      )
    })

    na_position_after <- which(is.na(xafty_column))

    if(sum(na_position_after) > sum(na_position)) {
      return(datetimes)
    } else {
      names(xafty_column) <- NULL
      as.POSIXct(xafty_column, tryFormats = tryFormats)
    }
  }

}

#' @title Build a List of Test Results
#' @description
#' This function is supposed to be an upgrade to the function check_validity(). The xafty_list should make it easy
#' to efficiently handle all checks and filters for each test within a dashboard context.
#'
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A validation table that stores the rules that the check table will be checked against.
#' @param align_columns Boolean. Whether to align column types before checking for xafty rules. Does not coerce values to NA!
#' @param meta_tests_name Character. Name of the list item that stores all meta_tests. The parameter is there to help avoid naming
#' conflicts with column names from the check table or validity table.
#' @param check_names Boolean. Adds a meta test that checks whether all column names of the validity table are present in
#' the check table.
#' @param check_number Boolean. Adds a meta test that checks whether the number of columns in the check table are equal or
#' larger than the columns in the validity table.
#' @returns A list.
#' @export
build_xafty_list <- function(check_table, validity_table, align_columns = TRUE,
                             meta_tests_name = "meta_tests", check_names = TRUE, check_number = TRUE) {

  validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table,
                                                  multiple = "remove")

  if(align_columns) {
    check_table <- align_column_types(check_table = check_table, validity_table = validity_table,
                                                      force_type = FALSE)
  }

  base_column_list <- list()

  if (check_names || check_number) {

    base_column_list[[meta_tests_name]] <- list()
  }

  ## Check for column names
  if (check_names) {

    check_column_names_result <- check_column_names(check_table = check_table, validity_table = validity_table,
                                                    check_type = "presence")
    test_name <- "xafty_column_names"

    filter_names <- filter_column_names(check_table = check_table, validity_table = validity_table)

    base_column_list[[meta_tests_name]][[test_name]] <- list(
      column_name = meta_tests_name,
      rule_syntax = test_name,
      values = names(validity_table),
      test_result = check_column_names_result$Check_Result,
      check_function = check_column_names,
      filter_function = filter_column_names,
      filter_result = filter_names,
      message = check_column_names_result$Message
    )
  }

  ## Check for column number
  if (check_number) {

    check_column_number_result <- check_column_number(check_table = check_table, validity_table = validity_table,
                                                      check_type = "larger")
    test_name <- "xafty_column_number"

    base_column_list[[meta_tests_name]][[test_name]] <- list(
      column_name = meta_tests_name,
      rule_syntax = test_name,
      values = NULL,
      test_result = check_column_number_result$Check_Result,
      check_function = check_column_number,
      message = check_column_number_result$Message
    )


  }
  colnames_validity <- colnames(validity_table)
  n_col <- length(colnames_validity)

  xafty_syntax <- xafty_rules_table$syntax

  xafty_pairs <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  for (col in colnames_validity) {
    if (!(col %in% colnames(check_table))) {
      next
    }

    base_column_list[[col]] <- list()

    xafty_rules_col <- names(xafty_pairs)[col == xafty_pairs]
    n_xafty_rules_col <- length(xafty_rules_col)

    single_col_check_table <- check_table[, col, drop = FALSE]
    single_col_validity_table <- validity_table[, col, drop = FALSE]

    for (i in seq(n_xafty_rules_col)) {
      single_rule <- xafty_rules_col[i]
      single_xafty_pair <- xafty_pairs[names(xafty_pairs) == single_rule & xafty_pairs == col]

      xafty_type <- xafty_rules_table$type[xafty_rules_table$syntax == single_rule]
      xafty_check_function <- xafty_rules_table$check_function[xafty_rules_table$syntax == single_rule][[1]]
      xafty_filter_function <-  xafty_rules_table$filter_function[xafty_rules_table$syntax == single_rule][[1]]
      change_type_function <- xafty_rules_table$change_type_function[xafty_rules_table$syntax == single_rule][[1]]
      xafty_values <- NULL

      if (xafty_type == "value") {
        xafty_values <- obtain_values_in_validity(
          validity_table = validity_table,
          xafty_pair = single_xafty_pair
        )
      }

      xafty_filter_result <- NULL
      if (!is.null(xafty_filter_function)) {
        xafty_filter_result <- xafty_filter_function(check_table = check_table, validity_table = validity_table,
                                                          filter_column = single_xafty_pair, xafty_rule = single_rule,
                                                          xafty_values = xafty_values)
      }


      test_result_row <- xafty_check_function(check_table = single_col_check_table,
        validity_table = single_col_validity_table)

      test_result <- test_result_row$Check_Result
      message <- test_result_row$Message

      single_rule_no_syntax <- sub("##!!", "", single_rule)

      base_column_list[[col]][[single_rule_no_syntax]] <- list(
        column_name = single_xafty_pair[[1]],
        rule_syntax = single_rule,
        rule_type = xafty_type,
        values = xafty_values,
        test_result = test_result,
        check_function = xafty_check_function,
        filter_function = xafty_filter_function,
        change_type_function = change_type_function,
        filter_result = xafty_filter_result,
        message = message
      )
    }
  }

  base_column_list
}

#' @title Create a Readable Table from a xafty List
#' @description
#' The function creates a rather readable output from a xafty list, that can be used in a summary context.
#'
#' @param xafty_list A list. The list should be the return value from the function build_xafty_list()
#' @returns A Dataframe.
#' @export
build_xafty_test_table <- function(xafty_list) {
  colnames <- names(xafty_list)
  n_colnames <- length(colnames)

  list_tmp <- list()

  for (i in seq(n_colnames)) {
    col <- colnames[i]

    n_xafty_rules <- length(xafty_list[[col]])

    array_tmp <- array(dim = c(n_xafty_rules, 2), dimnames = list(NULL, c("rule", "test_result")))

    for (j in seq(n_xafty_rules)) {
      array_tmp[j, "rule"] <- xafty_list[[col]][[j]]$rule_syntax
      array_tmp[j, "test_result"] <- xafty_list[[col]][[j]]$test_result
    }

    list_tmp[[i]] <- data.frame(
      "column" = rep(col, n_xafty_rules), "rule" = array_tmp[, "rule"],
      "test_result" = as.logical(array_tmp[, "test_result"])
    )
  }

  df <- do.call(rbind, list_tmp)

  row.names(df) <- NULL

  df
}

is_valid_email <- function(emails) {

  email_regex <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"

  grepl(email_regex, emails)

}


get_column_rules_items <- function(xafty_list, column, rule = NULL, item = NULL) {

  if(!is.null(rule) & !is.null(item))  {
    xafty_list_reduced <-  xafty_list[[column]][[rule]][[item]]
  } else if (!is.null(rule) & is.null(item)) {
    xafty_list_reduced <-  xafty_list[[column]][[rule]]
  } else if (is.null(rule) & is.null(item)) {
    xafty_list_reduced <- xafty_list[[column]]
  } else {
    xafty_list_reduced <- xafty_list
  }

  xafty_list_reduced

}

get_rules_xafty_list <- function(xafty_list, columns = NULL) {

  if(is.null(columns)) {
    xafty_list_names <- names(xafty_list)
    xafty_rules_list <- lapply(xafty_list_names, \(x) {
          names(get_column_rules_items(xafty_list, x))
      })

    rules_list_out <- setNames(xafty_rules_list, xafty_list_names)
  } else {

    xafty_rules_list <- lapply(columns, \(x) {
      names(get_column_rules_items(xafty_list, x))
    })
    rules_list_out <- setNames(xafty_rules_list, columns)

  }

  rules_list_out

}

#' @title Gets the Desired Items from a Column
#' @param xafty_list A List Created by build_xafty_list()
#' @param column The column from which the items should be taken?
#' @param item The desired item from the xafty_list
#' @export
get_xafty_list_items <- function(xafty_list, column, item) {

  xafty_rule_list <- get_rules_xafty_list(xafty_list, columns = column)
  xafty_rules <- xafty_rule_list[[column]]

  xafty_item_list <- lapply(xafty_rules, \(x){
    get_column_rules_items(xafty_list, column = column, rule =  x, item = item)
    })

  setNames(xafty_item_list, xafty_rules)
}

