test_that("Filter for values are correctly identified", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "yes", "no")))

  validity_table <- data.frame("Name" = c("##!!text", "##!!strictpattern", "a", NA),
                               "Has_Birthday" = c("##!!factor", "##!!rowpattern", "n", "y"))

  check_result <- filter_column_patterninput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(check_result, c(TRUE, TRUE, TRUE))
  expect_equal(length(check_result), 3)

})

test_that("Filter for values are correctly identified", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "maybe", "no")))

  validity_table <- data.frame("Name" = c("##!!text", "##!!strictpattern", "a", NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictpattern", "n", "o"))

  check_result <- filter_column_patterninput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(check_result, c(TRUE, FALSE, TRUE))
  expect_equal(length(check_result), 3)

})

test_that("Filter for values are correctly identified for NA Values", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", "maybe", NA)))

  validity_table <- data.frame("Name" = c("##!!text", "##!!strictpattern", "a", NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictpattern", "n", "o"))

  check_result <- filter_column_patterninput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(check_result, c(TRUE, FALSE, TRUE))
  expect_equal(length(check_result), 3)

})

test_that("Filter for values are correctly identified for NA Values", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", NA, NA)))

  validity_table <- data.frame("Name" = c("##!!text", "##!!strictpattern", "a", NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictpattern", "n", "o"))

  check_result <- filter_column_patterninput(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(check_result, c(TRUE, TRUE, TRUE))
  expect_equal(length(check_result), 3)

})

test_that("Several rules in one column throws an error", {

  check_table <- data.frame("Name" = as.character(c("David", "Diana", "Marcel")),
                            "Has_Birthday" = factor(c("no", NA, NA)))

  validity_table <- data.frame("Name" = c("##!!text", "##!!strictpattern", "a", NA),
                               "Has_Birthday" = c("##!!factor", "##!!strictpattern", "n", "##!!eachpattern"))

 expect_error(check_result <- filter_column_patterninput(check_table = check_table, validity_table = validity_table,
                                                         filter_column = "Has_Birthday"))

})
