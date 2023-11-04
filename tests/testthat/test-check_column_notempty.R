test_that("Function returns true when no empty values are found", {

  check_table <- data.frame("Name" = c("David", "Diana", "Marcel"),
                            "Age" = c(22, 18, NA))

  validity_table <- data.frame("Name" = c("##!!text", "##!!notempty"), "Age" = c("##!!number", NA))

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$Columns, NA)

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_true(check_result)

})

test_that("Function correctly detects empty values for one column", {

  check_table <- data.frame("Name" = c("David", "Diana", NA),
                            "Age" = c(22, 18, 25))

  validity_table <- data.frame("Name" = c("##!!text", "##!!notempty"), "Age" = c("##!!number", NA))

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Name")

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_false(check_result)

})

test_that("Function correctly detects empty values for two column", {

  check_table <- data.frame("Name" = c("David", "Diana", NA),
                            "Age" = c(22, 18, NA),
                            "Error" = c("", "", ""))

  validity_table <- data.frame("Name" = c("##!!text", "##!!notempty"), "Age" = c("##!!number", "##!!notempty"))

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Name, Age")

  check_result <- check_column_notempty(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_false(check_result)

})
