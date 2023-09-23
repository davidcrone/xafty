test_that("Correct data type of check table returns true", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
                            )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$Columns, NA)

})

test_that("'Expected date but wasn't found' returns false", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Birthday")

})

test_that("'Expected datetime but wasn't found' returns false", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.character(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Arrival_Time")

})

test_that("'Expected character column type but wasn't found' returns false", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Name")

})

test_that("'Expected double but wasn't found' returns false", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.character(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = factor(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Age")

})

test_that("'Expected factor but wasn't found' returns false", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Age" = as.double(c(22, 18, 25)),
                            "Birthday" = as.Date(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no")),
                            "Arrival_Time" = as.POSIXct(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Has_Birthday")

})

test_that("Every columns with unexpected data type are detected", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.character(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no")),
                            "Arrival_Time" = as.character(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  check_result <- check_column_types(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$Columns, "Name, Age, Birthday, Has_Birthday, Arrival_Time")

})
