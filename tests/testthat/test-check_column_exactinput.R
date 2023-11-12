test_that("That 'strict exact' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes", "maybe")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_true(check_result)

})

test_that("That 'strict exact' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!strictexact", "no")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Has_Birthday")

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_false(check_result)

})

test_that("That 'any exact' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!anyexact", "David", "Diana", "Thomas"),
    "Has_Birthday" = c("##!!factor")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("That 'any exact' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!anyexact", "Martin", "Dav"),
    "Has_Birthday" = c("##!!factor")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Name")
})

test_that("That 'each exact' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel", "Thomas")),
    "Has_Birthday" = factor(c("no", "yes", "no", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!eachexact", "David", "Diana"),
    "Has_Birthday" = c("##!!factor")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("That 'each exact' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel", "Thomas")),
    "Has_Birthday" = factor(c("no", "yes", "no", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text"),
    "Has_Birthday" = c("##!!factor", "##!!eachexact", "yes", "no", "maybe")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Has_Birthday")
})

test_that("A combination of strict input rules works", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel", "Thomas")),
    "Has_Birthday" = factor(c("no", "yes", "no", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text"),
    "Has_Birthday" = c("##!!factor", "##!!eachexact", "yes", "##!!strictexact", "yes", "no", "maybe")
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("Function returns an error when no values for exact rules are found", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!anyexact"),
    "Has_Birthday" = c("##!!factor", "##!!strictexact")
  )

  check_results <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_false(check_results$Check_Result)
})

test_that("Function returns a warning when no corresponding rules are found", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text"),
    "Has_Birthday" = c("##!!factor")
  )

  test_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_equal(test_result$columns, NA)
  expect_true(grepl("Warning", test_result$Message))
})


test_that("That 'strict exact' functions the same with NA values inbetween", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text"),
    "Has_Birthday" = c("##!!factor", "##!!strictexact", NA, NA, "no", NA, NA, "yes", NA, NA)
  )

  check_result <- check_column_exactinput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})
