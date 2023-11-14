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

test_that("Multiple regex values below the rule work as expected", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", NA , NA, NA),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-3]", "^W[4-5]"))

  new_validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table)

  expect_true(check_column_names(check_table = check_table, validity_table = new_validity_table, simply = TRUE))
  expect_true(check_column_types(check_table = check_table, validity_table = new_validity_table, simply = TRUE))

})

test_that("No provided regular expression raises a warning", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", NA ),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns"))

  expect_warning(add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table))

})

test_that("No ##!!regexcolumns rule found in validity table returns the validity table", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number"),
                               Wagon_Desing = c("##!!number"))

  new_validty_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table)


  expect_identical(validity_table, new_validty_table)
})


test_that("Parameter keep mutliple matched columns keeps all duplicated columns", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", "##!!regexcolumns", "^W[1-9]"),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"))

  new_validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table,
                                                      multiple = "keep")

  expect_equal(ncol(new_validity_table), 6)

})

test_that("Passing the wrongly named parameter, raises an error", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number", "##!!regexcolumns", "^W[4-9]"),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^W[1-9]"))

  expect_error(add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table,
                                                      multiple = "sort"))

})

test_that("No matches for regular expressions raises a warning", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number",NA, NA),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^T[1-9]"))

  expect_warning(add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table,
                                                      multiple = "keep"))

})


test_that("Having only one of two regular expression that matches columns works", {

  check_table <- data.frame(W501 = c(0, 1, 0),
                            W4051 = c(1, 1, 1),
                            W301 = c(0.1, 0.9, 0),
                            LKW = c(1, 0, 0))
  validity_table <- data.frame(LKW = c("##!!number",NA, NA, NA),
                               Wagon_Desing = c("##!!number", "##!!regexcolumns", "^T[1-9]", "^W[1-9]"))

  new_validity_table <- add_regex_columns_to_validity(check_table = check_table, validity_table = validity_table,
                                               multiple = "keep")

  expect_true(check_column_names(check_table = check_table, validity_table = new_validity_table, simply = TRUE))
  expect_true(check_column_types(check_table = check_table, validity_table = new_validity_table, simply = TRUE))

})
