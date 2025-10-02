
test_that("curly variables are retrieved from the curly brackets", {
  test_values <- c("normal_character", "{xafty_state}", "{another.xafty_state}", "{inval{_name}", "{}",
                   "name.{variable}", "name_{variable}", "name{variable}", "name.variable", "[name.variable]")
  test_variables <- vapply(test_values, get_braced_variable, FUN.VALUE = character(1), USE.NAMES = FALSE)
  expected_characters <- c(NA, "xafty_state", "another.xafty_state", "_name", NA, "variable", "variable", "variable", NA, NA)
  expect_identical(test_variables, expected_characters)
})

test_that("xafty states are correctly identified", {
  test_values <- c("normal_character", "{xafty_state}", "{another.xafty_state}", "{inval{_name}", "{}",
                   "name.{variable}", "name_{variable}", "name{variable}", "name.variable", "[name.variable]")
  test_logical <- vapply(test_values, contains_state, logical(1), USE.NAMES = FALSE)
  expect_logical <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_identical(test_logical, expect_logical)
})

test_that("xafty objects are correctly identified", {
  test_logical <- vapply(c("normal_character", "[xafty_object]", "[another.xafty_state]", "[inval{_name]", "[]"),
                         is_xafty_object_variable, logical(1), USE.NAMES = FALSE)
  expect_logical <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
  expect_identical(test_logical, expect_logical)
})

test_that("squared variables are retrieved from the squared brackets", {
  test_characters <- c("[xafty_object]", "[another.xafty_state]", "[inval[_name]")
  test_variables <- vapply(test_characters, get_squared_variable, FUN.VALUE = character(1), USE.NAMES = FALSE)
  expected_characters <- c("xafty_object", "another.xafty_state", "inval[_name")
  expect_identical(test_variables, expected_characters)
})

