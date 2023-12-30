#' @title Check Column Classes
#'
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @param simply Boolean. Changes the return value of the function to a single logical vector of length 1.
#' @export
check_column_types <- function(check_table, validity_table, simply = FALSE) {

  check_table_columns <- colnames(check_table)

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)
  xafty_type_pairs <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_data_types)
  n_xafty_pairs <- length(xafty_type_pairs)

  list_result <- list()

  for (i in seq(n_xafty_pairs)) {

    check_table_column <- xafty_type_pairs[i]
    xafty_data_type <- names(check_table_column)

    if (check_table_column %in% check_table_columns) {

      list_result[[i]] <- switch(xafty_data_type,
        "##!!text" = is.character(check_table[[check_table_column]]),
        "##!!date" = inherits(check_table[[check_table_column]], "Date"),
        "##!!number" = is.numeric(check_table[[check_table_column]]),
        "##!!factor" = is.factor(check_table[[check_table_column]]),
        "##!!datetime" = inherits(check_table[[check_table_column]], "POSIXct")
      )
      names(list_result[[i]]) <- check_table_column
    }
  }

  result_unlisted <- unlist(list_result)

  if (all(result_unlisted)) {
    result <- TRUE
    if (simply) {
      return(result)
    }
    message <- paste0("ALL GOOD!")
    columns <- NA
  } else {
    result <- FALSE

    if (simply) {
      return(result)
    }

    rule_to_type_table <- data.frame(rule = c('##!!text', '##!!date', '##!!number', '##!!factor', '##!!datetime'),
                                     type = c("character", "Date", "numeric", "factor", "POSIXct"))
    wrong_classes_names <- names(result_unlisted)[!result_unlisted]
    actual_type <- sapply(wrong_classes_names, \(col) class(check_table[[col]]))
    expected_type <- sapply(wrong_classes_names, \(col) {
      rule <- names(xafty_type_pairs)[xafty_type_pairs == col]
      rule_to_type_table$type[rule_to_type_table$rule == rule]
      })

    type_report <- paste0("Column: '", wrong_classes_names, "' Expected: '", expected_type, "' Actual: '", actual_type, "'",
                   collapse = "; ")

    wrong_classes_collapsed <- paste0(wrong_classes_names, collapse = ", ")
    columns <- wrong_classes_collapsed
    message <- paste0("Rule Broken: Column Type. Following columns differ from expectation: ", type_report)
  }

  data.frame("Check" = "Column Types", "Check_Result" = result, "Message" = message, "Columns" = columns)
}

#' @title Align Column Classes with Validity Table
#'
#' @description
#' This function changes the column classes according to the data types of the validity table
#'
#' @param check_table Data table. A table that will be will have its classes aligned specified in the validity table
#' @param validity_table Data table. A validity table that holds the class information for alignment
#' @param date_origin Character. A date string for date conversion giving the number of days since e.g. "1900-01-01". This
#' is only necessary if the excel date is stored as numeric: (32768, 35981). For more information see: ?as.Date
#' @param tryFormats_Date Character vector. Date formats that should be use to try to convert to date
#' @param tryFormats_POSIXct Character vector. POSIXct formats that should be use to try to convert to POSIXct
#' @param tz Timezone for the POSIXct values. Default is UTC
#' @param force_type Boolean. Whether to force the type conversion even if it introduces NAs during type conversion. If TRUE
#' the function keeps the column as is.
#' @export
align_column_types <- function(check_table, validity_table, force_type = TRUE,
                               date_origin = "1899-12-30",
                               tryFormats_Date = c("%d.%m.%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"),
                               tryFormats_POSIXct = c("%Y-%m-%d %H:%M:%OS",
                                                      "%Y/%m/%d %H:%M:%OS",
                                                      "%Y-%m-%d %H:%M",
                                                      "%Y/%m/%d %H:%M",
                                                      "%Y-%m-%d",
                                                      "%Y/%m/%d"),
                               tz = "") {
  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  for (i in colnames(check_table)) {
    if (i %in% colnames(validity_table)) {
      values_column <- validity_table[, i]
      logical_data_type <- xafty_data_types %in% values_column

      if (sum(logical_data_type, na.rm = TRUE) > 1) stop(paste0("Validity column '", i, "' has more than one data type"))

      xafty_data_type <- xafty_data_types[logical_data_type]

      switch(xafty_data_type,
        "##!!text" = check_table[, i] <- as.character(check_table[[i]]),
        "##!!date" = check_table[, i] <- as.Date_xafty(check_table[[i]],
                                         force_type = force_type, date_origin = date_origin, tryFormats = tryFormats_Date
        ),
        "##!!number" = check_table[, i] <- as.numeric_xafty(check_table[[i]], force_type = force_type),
        "##!!factor" = check_table[, i] <- as.factor(check_table[[i]]),
        "##!!datetime" = check_table[, i] <- as.POSIXct_xafty(check_table[[i]], force_type = force_type,
                                             tryFormats = tryFormats_POSIXct, tz = tz)
      )
    }
  }

  check_table
}
