test_that("Presence of all column names is correctly detected", {
  check_table <- data.frame(
    "Name" = c("David", "Diana", "Marcel"),
    "Age" = c(22, 18, 25)
  )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "presence")

  expect_true(checK_result$Check_Result)
  expect_equal(checK_result$Columns, NA)

  expect_true(check_column_names(
    check_table = check_table, validity_table = validity_table,
    check_type = "presence", simply = TRUE
  ))
})

test_that("Non presence of a column is correctly detected", {
  check_table <- data.frame(
    "Name" = c("David", "Diana", "Marcel"),
    "Age" = c(22, 18, 25)
  )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number", "Birthday" = "##!!date")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "presence")

  expect_false(checK_result$Check_Result)
  expect_equal("Birthday", checK_result$Columns)

  expect_false(check_column_names(
    check_table = check_table, validity_table = validity_table,
    check_type = "presence", simply = TRUE
  ))
})

test_that("Non presence of several columns is correctly detected", {
  check_table <- data.frame("Name" = c("David", "Diana", "Marcel"))
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number", "Birthday" = "##!!date")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "presence")

  expect_false(checK_result$Check_Result)
  expect_equal("Age, Birthday", checK_result$Columns)

  expect_false(check_column_names(
    check_table = check_table, validity_table = validity_table,
    check_type = "presence", simply = TRUE
  ))
})


test_that("Correct order of columns is correctly detected", {
  check_table <- data.frame(
    "Name" = c("David", "Diana", "Marcel"),
    "Age" = c(22, 18, 25),
    "Birthday" = c("2000-12-12", "1999-02-02", "1989-02-28")
  )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number", "Birthday" = "##!!date")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "order")

  expect_true(checK_result$Check_Result)
  expect_equal(checK_result$Columns, NA)

  expect_true(check_column_names(
    check_table = check_table, validity_table = validity_table,
    check_type = "order", simply = TRUE
  ))
})

test_that("Correct order of columns is correctly detected irrespective of position", {
  check_table <- data.frame(
    "ID" = c(3544, 5623, 5234),
    "Name" = c("David", "Diana", "Marcel"),
    "Age" = c(22, 18, 25),
    "Birthday" = c("2000-12-12", "1999-02-02", "1989-02-28")
  )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number", "Birthday" = "##!!date")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "order")

  expect_true(checK_result$Check_Result)
  expect_equal(checK_result$Columns, NA)
})

test_that("Incorrect order of columns is correctly detected", {
  check_table <- data.frame(
    "ID" = c(3544, 5623, 5234),
    "Name" = c("David", "Diana", "Marcel"),
    "Age" = c(22, 18, 25),
    "Birthday" = c("2000-12-12", "1999-02-02", "1989-02-28")
  )
  validity_table <- data.frame("Name" = "##!!text", "Birthday" = "##!!date", "Age" = "##!!number")

  checK_result <- check_column_names(check_table = check_table, validity_table = validity_table, check_type = "order")

  expect_false(checK_result$Check_Result)
  expect_equal(checK_result$Message, "All columns are present but not in the specified order")

  expect_false(check_column_names(
    check_table = check_table, validity_table = validity_table,
    check_type = "order", simply = TRUE
  ))
})

test_that("filter_column_names finds all missing columns in the tables", {

  check_table <- data.frame(
    "ID" = c(3544, 5623, 5234),
    "Name" = c("David", "Diana", "Marcel"),
    "Birthday" = c("2000-12-12", "1999-02-02", "1989-02-28")
  )
  validity_table <- data.frame("Name" = "##!!text", "Age" = "##!!number")

  filter_result <- filter_column_names(check_table = check_table, validity_table = validity_table)

  missing_col_check <- colnames(check_table[filter_result$colnames_check_table])
  missing_col_vald <- colnames(validity_table[filter_result$colnames_validity_table])

  expect_equal(missing_col_check, c("ID", "Birthday"))
  expect_equal(missing_col_vald, c("Age"))

})
