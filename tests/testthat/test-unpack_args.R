test_that("unpack unpacks a function with no argument correctly", {
  test_function <- function() {
    data.frame(a = c("1", "s"))
  }
  list_unpacked_test <- test_unpack_args(test_function())

  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list()
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack unpacks a function with one empty argument correctly", {
  test_function <- function(arg) {
    data.frame(a = c("1", "s"))
  }
  list_unpacked_test <- list_unpacked_test <- test_unpack_args(test_function(arg = TRUE))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(arg = TRUE)
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack unpacks a function with the first argument being a data.frame correctly", {
  test_function <- function(arg) {
    data.frame(a = c("1", "s"))
  }
  data <- data.frame(b = c(1, 2))
  list_unpacked_test <- test_unpack_args(test_function(arg = data))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(arg = data.frame(b = c(1, 2))
    )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack unpacks correctly a function with the first and scond argument being a data.frame", {
  test_function <- function(arg1, arg2) {
    data.frame(a = c("1", "s"))
  }
  data1 <- data.frame(b = c(1, 2))
  data2 <- data.frame(c = c(3, 4))
  list_unpacked_test <- test_unpack_args(test_function(arg1 = data1, arg2 = data2))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(arg1 = data.frame(b = c(1, 2)),
                arg2 = data.frame(c = c(3, 4))
    )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack works with a function as an argument", {
  test_function <- function(func) {
    data <- c(1:5)
    data.frame(a = func(data))
  }
  list_unpacked_test <- test_unpack_args(test_function(func = mean))

  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(
      func = mean
      )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack unpacks a function correctly with an unamed (dots) argument", {
  test_function <- function(arg1, ...) {
    data <- list(...)[[1]]
    data.frame(a = data)
  }
  list_unpacked_test <- test_unpack_args(test_function(arg1 = TRUE, ... = 1:3))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(
      arg1 = TRUE,
      ... = c(1L, 2L, 3L))
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack unpacks function call as the default argument", {

  get_value <- function() {
    TRUE
  }
  get_data <- function() {
    1:3
  }
  test_function <- function(arg1 = get_data(), arg2 = get_value()) {
    data <- arg1
    data.frame(a = data)
  }
  list_unpacked_test <- test_unpack_args(test_function())
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(
      arg1 = c(1L, 2L, 3L),
      arg2 = TRUE)
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack assigns correctly positional arguments to the correct named argument ", {
  test_function <- function(arg1, arg2) {
    data <- arg1 + arg2
    data.frame(a = data)
  }
  list_unpacked_test <- test_unpack_args(test_function(1:3, 1:3))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(
      arg1 = c(1L, 2L, 3L),
      arg2 = c(1L, 2L, 3L)
    )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack assigns named arguments in a different order", {
  test_function <- function(arg1, arg2) {
    data <- arg1 + arg2
    data.frame(a = data)
  }
  list_unpacked_test <- test_unpack_args(test_function(arg2 = 2:4, arg1 = 1:3))
  list_unpack_expected <- list(
    fun_name = "test_function",
    fun = test_function,
    args = list(
      arg1 = c(1L, 2L, 3L),
      arg2 = c(2L, 3L, 4L)
    )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack keeps name of the argument that are passed to the dots argument", {
  test_get_function <- function(arg1, ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  list_unpacked_test <- test_unpack_args(test_get_function(arg1 = list(1, 2), comment = "clear", 1:3))
  list_unpack_expected <- list(
    fun_name = "test_get_function",
    fun = test_get_function,
    args = list(
      arg1 = list(1, 2),
      comment = "clear",
      1:3)
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})

test_that("unpack can unpack a mix of different ways to pass an argument in the same call", {
  get_value <- function() {
    TRUE
  }
  test_get_function <- function(data1, pos1, fun1, default1 = get_value(), ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  list_unpacked_test <- test_unpack_args(test_get_function(data1 = list(1, 2), FALSE, fun1 = median, comment = "clear", data2 = 1:3))
  list_unpack_expected <- list(
    fun_name = "test_get_function",
    fun = test_get_function,
    args = list(
      data1 = list(1, 2),
      pos1 = FALSE,
      fun1 = median,
      default1 = TRUE,
      comment = "clear",
      data2 = 1:3
    )
  )
  expect_identical(list_unpacked_test, list_unpack_expected)
})
