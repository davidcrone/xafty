test_that("That 'strict pattern' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!strictpattern", "a", NA),
    "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_true(check_result)

})

test_that("That 'strict pattern' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", "##!!strictpattern", "i", NA),
    "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Name")

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table, simply = TRUE)

  expect_false(check_result)
})


test_that("That 'row pattern' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!rowpattern", "n", "y", "t")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("That 'row pattern' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!rowpattern", "n", "o")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Has_Birthday")
})

test_that("That 'any pattern' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!anypattern", "n", "o")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("That 'any pattern' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel")),
    "Has_Birthday" = factor(c("no", "yes", "no"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!anypattern", "x")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Has_Birthday")
})

test_that("That 'each pattern' is correctly flagged when fulfilled", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel", "Thomas", "Claire")),
    "Has_Birthday" = factor(c("no", "yes", "no", "maybe", "never"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!eachpattern", "n", "o", "m", "e")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_true(check_result$Check_Result)
  expect_equal(check_result$columns, NA)
})

test_that("That 'any pattern' is correctly flagged when broken", {
  check_table <- data.frame(
    "Name" = as.character(c("David", "Diana", "Marcel", "Thomas", "Claire")),
    "Has_Birthday" = factor(c("no", "yes", "no", "maybe", "never"))
  )

  validity_table <- data.frame(
    "Name" = c("##!!text", NA, NA, NA, NA, NA),
    "Has_Birthday" = c("##!!factor", "##!!eachpattern", "n", "o", "m", "t")
  )

  check_result <- check_column_patterninput(check_table = check_table, validity_table = validity_table)

  expect_false(check_result$Check_Result)
  expect_equal(check_result$columns, "Has_Birthday")
})
