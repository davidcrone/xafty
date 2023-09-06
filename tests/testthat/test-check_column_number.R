test_that("Correct number of columns is correctly detected", {

  check_table <- data.frame("Name" = c("David", "Diana", "Marcel"),
                            "Age" = c(22, 18, 25))
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number")

  checK_result <- check_column_number(check_table = check_table, validity_table = validity_table)

  expect_true(checK_result$Check_Result)
  expect_equal(checK_result$Columns, NA)

})

test_that("Incorrect number of columns is correctly detected", {

  check_table <- data.frame("Name" = c("David", "Diana", "Marcel"),
                            "Age" = c(22, 18, 25))
  validity_table <- data.frame("Name" = "##!!text")

  checK_result <- check_column_number(check_table = check_table, validity_table = validity_table)

  expect_false(checK_result$Check_Result)
  expect_equal(checK_result$Columns, NA)

})
