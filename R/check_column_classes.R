
#' @title Check Column Classes
#'
#' @param check_table Data Frame. The table that will be checked against the specified rules in the validity table.
#' @param validity_table Data Frame. A table that stores the rules by which the check table is compared to.
#' @export
check_column_classes <- function(check_table, validity_table) {

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)
  col_names_check_table <- colnames(check_table)
  list_result <- list()

  for (i in col_names_check_table) {

    if (i %in% colnames(validity_table)) {

      values_column <- validity_table[, i]
      logical_data_type <-  xafty_data_types %in% values_column
      xafty_data_type <- xafty_data_types[logical_data_type]

      list_result[[i]] <- switch (xafty_data_type,
                          "##!!text" = is.character(check_table[[i]]),
                          "##!!date" = inherits(check_table[[i]], "Date"),
                          "##!!number" = is.numeric(check_table[[i]]),
                          "##!!factor" = is.factor(check_table[[i]]),
                          "##!!datetime" = inherits(check_table[[i]], "POSIXct")
                            )

    }

  }

  result_unlisted <- unlist(list_result)

  if (all(result_unlisted)) {

    result <- TRUE
    message <- paste0("ALL GOOD!")
    columns <- NA

  } else {
    # TODO: Add expected data type and found data tabe
    result <- FALSE
    wrong_classes_names <- names(result_unlisted)[!result_unlisted]
    wrong_classes_collapsed <- paste0(wrong_classes_names, collapse = ", ")
    columns <- wrong_classes_collapsed
    message <- paste0("Rule Broken: Column Classes. Following columns have the wrong class type: ")
  }

  data.frame("Check" = "Column Classes", "Check_Result" = result, "Message" = message, "Columns" = columns)

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
#'
#' @export
align_column_classes <- function(check_table, validity_table, date_origin = "1899-12-30") {

  xafty_syntax <- "##!!"
  possible_classes <- c("text", "date", "number", "factor", "datetime")
  xafty_data_types <- paste0(xafty_syntax, possible_classes)

  for (i in colnames(check_table)) {

    if (i %in% colnames(validity_table)) {

      values_column <- validity_table[, i]
      logical_data_type <-  xafty_data_types %in% values_column

      if (sum(logical_data_type, na.rm = TRUE) > 1) stop(paste0("Validity column '", i,  "' has more than one data type"))

      xafty_data_type <- xafty_data_types[logical_data_type]

      switch (xafty_data_type,
              "##!!text" = check_table[, i] <- as.character(check_table[[i]]),
              "##!!date" = check_table[, i] <- tryCatch(as.Date(check_table[[i]], tryFormats = c("%d.%m.%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")),
                                                       error = function(e) as.Date(as.numeric(check_table[[i]]), origin = date_origin)),
              "##!!number" = check_table[, i] <- as.numeric(check_table[[i]]),
              "##!!factor" = check_table[, i] <- as.factor(check_table[[i]]),
              "##!!datetime" = check_table[, i] <- as.POSIXct(check_table[[i]])
      )

    }

  }

  check_table

}

