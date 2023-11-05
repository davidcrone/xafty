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
  arg_names <- names(formals(check_validity))[!(names(formals(check_validity)) %in% c("check_table", "validity_table"))]

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
#' @return An equally length date vector, \code{NA} when the value could not be converted to date
as.Date_xafty <- function(dates, date_origin = "1899-12-30", tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d")) {
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

  print(class(xafty_column))

  as.Date(xafty_column, origin = "1970-01-01")
}

#' @title Check if Passed Values can be Parsed as Numeric
#' @param numbers Character vector of Numbers to be Parsed
#' @return An equally length Boolean vector whether the values can be parsed as numbers
is.numeric_xafty <- function(numbers) {
  xafty_column <- rep(TRUE, length(numbers))

  position_na <- which(is.na(numbers))
  position_na_conversion <- which(is.na(suppressWarnings(as.numeric(numbers))))

  position_not_numbers <- setdiff(position_na_conversion, position_na)

  xafty_column[position_not_numbers] <- FALSE

  xafty_column
}

#' @title Check if Passed Values can be Parsed as POSIXct
#' @param datetimes Character vector of date time values to be parsed
#' @param tz Timezone for the POSIXct values. Default is UTC
#' @return An equally length Boolean vector whether the values can be parsed as POSIXct
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
as.POSIXct_xafty <- function(datetimes, tz = "") {
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

  as.POSIXct(xafty_column)
}

build_xafty_list <- function(check_table, validity_table, xafty_rules_table) {
  colnames_validity <- colnames(validity_table)
  n_col <- length(colnames_validity)

  xafty_syntax <- xafty_rules_table$syntax

  xafty_pairs <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  base_column_list <- list()

  for (col in colnames_validity) {
    # TODO: What should happens when a column in the validity table is not present in the check table?
    if (!(col %in% colnames(check_table))) {
      warning(paste(col, "is not present in check_table"))
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
      xafty_check_function <- xafty_rules_table$check_function[xafty_rules_table$syntax == single_rule]
      xafty_values <- NULL

      if (xafty_type == "value") {
        xafty_values <- obtain_values_in_validity(
          validity_table = validity_table,
          xafty_pair = single_xafty_pair
        )
      }

      test_result <- xafty_check_function[[1]](check_table = single_col_check_table,
        validity_table = single_col_validity_table)$Check_Result

      base_column_list[[col]][[single_rule]] <- list(
        rule = single_rule,
        values = xafty_values,
        test_result = test_result,
        check_function = xafty_check_function,
        filter_function = NULL
      )
    }
  }

  base_column_list
}

build_xafty_test_table <- function(xafty_list) {
  colnames <- names(xafty_list)
  n_colnames <- length(colnames)

  list_tmp <- list()

  for (i in seq(n_colnames)) {
    col <- colnames[i]

    n_xafty_rules <- length(xafty_list[[col]])

    array_tmp <- array(dim = c(n_xafty_rules, 2), dimnames = list(NULL, c("rule", "test_result")))

    for (j in seq(n_xafty_rules)) {
      array_tmp[j, "rule"] <- xafty_list[[col]][[j]]$rule
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
