test_that("Filter for values are correctly identified", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "yes", "no")))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes", "maybe"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(TRUE, TRUE, TRUE))
  expect_equal(length(filter_result), 3)


})

test_that("Filter for values are correctly identified with NA", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "yes", NA)))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes", "NA"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(TRUE, TRUE, TRUE))
  expect_equal(length(filter_result), 3)


})

test_that("Filter for values are correctly identified with broken value", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "yes", "maybe")))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictexact", "no", "yes", "NA"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(TRUE, TRUE, FALSE))
  expect_equal(length(filter_result), 3)


})

test_that("Filter for values are correctly identified with two rules in one column", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "yes", "maybe")))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!anyexact", "no", "##!eachexact", "yes"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(TRUE, TRUE, FALSE))
  expect_equal(length(filter_result), 3)


})

test_that("Filter with all broken values work", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("maybe", "maybe", "maybe")))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!anyexact", "no", "##!eachexact", "yes"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(FALSE, FALSE, FALSE))
  expect_equal(length(filter_result), 3)


})

test_that("All values NA returns FALSE", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c(NA, NA, NA)))

  validity_table <- data.frame("Name" = c("##!!text", NA, NA, NA),
                               "Has_Birthday" = c("##!!factor", "##!!anyexact", "no", "yes"))

  filter_result <- filter_column_exactinput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(filter_result, c(FALSE, FALSE, FALSE))
  expect_equal(length(filter_result), 3)


})
