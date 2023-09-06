test_that("Correct classes of check table return true", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
                            )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_classes(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$Columns, NA)

})
