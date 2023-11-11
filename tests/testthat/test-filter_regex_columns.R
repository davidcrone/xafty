test_that("Correct columns are filtered through regular expressions", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", NA , NA),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"))

  new_validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table)

  expect_true(check_column_names(check_table = check_table, validity_table = new_validity_table, simply = TRUE))
  expect_true(check_column_types(check_table = check_table, validity_table = new_validity_table, simply = TRUE))

})

test_that("All columns as regrex is possible and does not throw an error", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0),
                            WLKW = c(0, 1, 0))
  validity_table <- data.frame(LKW = c("##!!number", "##!!regexcolumns", "^L"),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"),
                               WLKW = c("##!!number", NA, NA))

  new_validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table)

  expect_true(check_column_names(check_table = check_table, validity_table = new_validity_table, simply = TRUE))
  expect_true(check_column_types(check_table = check_table, validity_table = new_validity_table, simply = TRUE))

})

test_that("Warning by multiple selection of same columns is printed", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", "##!!regexcolumns", "^W[1-9]"),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"))

  expect_warning(add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table))

})

test_that("Multiple selction of columns by regex rules does not multiply columns", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", "##!!regexcolumns", "^W[1-9]"),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"))

 new_validity_table <-  suppressWarnings(add_regex_columns_to_validity(check_table = check_table,
                                                                       validity_table = validity_table))

 # Both LKW and Wagon_Design select the same three columns, therefore 3 columns is correct
 expect_length(colnames(new_validity_table), 3)

 expect_true(check_column_names(check_table = check_table, validity_table = new_validity_table, check_type = "presence",
                                simply = TRUE))
 expect_true(check_column_types(check_table = check_table, validity_table = new_validity_table, simply = TRUE))

})
