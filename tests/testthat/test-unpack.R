# test_that("unpack unpacks a function with no argument correctly", {
#   test_function <- function() {
#     data.frame(a = c("1", "s"))
#   }
#   quosure <- capture_quosure(test_function())
#   list_unpacked_test <- unpack(quosure, )
#
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(),
#     package = "unkown", # During testthat the envirpnment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "NULL",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "NULL",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 2L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack unpacks a function with one empty argument correctly", {
#   test_function <- function(arg) {
#     data.frame(a = c("1", "s"))
#   }
#   quosure <- capture_quosure(test_function(arg = TRUE))
#   list_unpacked_test <- unpack(quosure, link_type = "get")
#
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(arg = TRUE),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "logical",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "NULL",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 2L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack unpacks a function with the first argument being a data.frame correctly", {
#   test_function <- function(arg) {
#     data.frame(a = c("1", "s"))
#   }
#   data <- data.frame(b = c(1, 2))
#   quosure <- capture_quosure(test_function(arg = data))
#   list_unpacked_test <- unpack(quosure, link_type = "add")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(arg = data.frame(b = c(1, 2))
#     ),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "add",
#     first = list(
#       cols = "b",
#       class = "data.frame",
#       rows = 2L
#     ),
#     second = list(
#       cols = NULL,
#       class = "NULL",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 2L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack unpacks correctly a function with the first and scond argument being a data.frame", {
#   test_function <- function(arg1, arg2) {
#     data.frame(a = c("1", "s"))
#   }
#   data1 <- data.frame(b = c(1, 2))
#   data2 <- data.frame(c = c(3, 4))
#   quosure <- capture_quosure(test_function(arg1 = data1, arg2 = data2))
#   list_unpacked_test <- unpack(quosure, link_type = "join")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(arg1 = data.frame(b = c(1, 2)),
#                 arg2 = data.frame(c = c(3, 4))
#     ),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "join",
#     first = list(
#       cols = "b",
#       class = "data.frame",
#       rows = 2L
#     ),
#     second = list(
#       cols = "c",
#       class = "data.frame",
#       rows = 2L
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 2L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack raises an error when link type get does not return a data.frame", {
#   test_function <- function() {
#     list(a = c("1", "s"))
#   }
#   quosure <- capture_quosure(test_function())
#   unpacked <- unpack(quosure, link_type = "get")
#   expect_error(validate_link_type(link_type = "get", unpacked = unpacked))
# })
#
# test_that("unpack raises an error when in link type add function's first argument is not a data.frame", {
#   test_function <- function(table) {
#     data.frame(a = c("1", "s"))
#   }
#   table <- FALSE
#   quosure <- capture_quosure(test_function(table))
#   unpacked <- unpack(quosure, link_type = "add")
#   expect_error(evaluate_link_type(link_type = "add", unpacked = unpacked))
# })
#
# test_that("unpack raises an error when in link type join the function's first or second argument is not a data.frame", {
#   test_function <- function(left, right) {
#     data.frame(a = c("1", "s"))
#   }
#   left <- data.frame()
#   right <- "not"
#   quosure <- capture_quosure(test_function(left = left, right = right))
#   unpacked <- unpack(quosure, link_type = "join")
#   expect_error(validate_link_type(link_type = "join", unpacked = unpacked), regexp = "second")
#
#   quosure <- capture_quosure(test_function(left = right, right = left))
#   unpacked <- unpack(quosure, link_type = "join")
#   expect_error(validate_link_type(link_type = "join", unpacked = unpacked), regexp = "first")
# })
#
# test_that("unpack works with a function as an argument", {
#   test_function <- function(func) {
#     data <- c(1:5)
#     data.frame(a = func(data))
#   }
#   quosure <- capture_quosure(test_function(func = mean))
#   list_unpacked_test <- unpack(quosure, link_type = "get")
#
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(
#       func = mean
#       ),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "function",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "NULL",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 1L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack unpacks a function correctly with an unamed (dots) argument", {
#   test_function <- function(arg1, ...) {
#     data <- list(...)[[1]]
#     data.frame(a = data)
#   }
#   quosure <- capture_quosure(test_function(arg1 = TRUE, ... = 1:3))
#   list_unpacked_test <- unpack(quosure, link_type = "get")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(
#       arg1 = TRUE,
#       ... = c(1L, 2L, 3L)),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "logical",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 3L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack unpacks function call as the default argument", {
#
#   get_value <- function() {
#     TRUE
#   }
#   get_data <- function() {
#     1:3
#   }
#   test_function <- function(arg1 = get_data(), arg2 = get_value()) {
#     data <- arg1
#     data.frame(a = data)
#   }
#   quosure <- capture_quosure(test_function())
#   list_unpacked_test <- unpack(quosure, link_type = "get")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(
#       arg1 = c(1L, 2L, 3L),
#       arg2 = TRUE),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "logical",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 3L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack assigns correctly positional arguments to the correct named argument ", {
#   test_function <- function(arg1, arg2) {
#     data <- arg1 + arg2
#     data.frame(a = data)
#   }
#   quosure <- capture_quosure(test_function(1:3, 1:3))
#   list_unpacked_test <- unpack(quosure, "get")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(
#       arg1 = c(1L, 2L, 3L),
#       arg2 = c(1L, 2L, 3L)),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 3L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack assigns named arguments in a different order", {
#   test_function <- function(arg1, arg2) {
#     data <- arg1 + arg2
#     data.frame(a = data)
#   }
#   quosure <- capture_quosure(test_function(arg2 = 2:4, arg1 = 1:3))
#   list_unpacked_test <- unpack(quosure, "get")
#   list_unpack_expected <- list(
#     fun_name = "test_function",
#     fun = test_function,
#     args = list(
#       arg1 = c(1L, 2L, 3L),
#       arg2 = c(2L, 3L, 4L)),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "integer",
#       rows = NULL
#     ),
#     output = list(
#       cols = "a",
#       class = "data.frame",
#       rows = 3L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack keeps name of the argument that are passed to the dots argument", {
#   test_get_function <- function(arg1, ...) {
#     data.frame(a = c(1:10),
#                b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
#   }
#   quosure <- capture_quosure(test_get_function(arg1 = list(1, 2), comment = "clear", 1:3))
#   list_unpacked_test <- unpack(quosure, "get")
#   list_unpack_expected <- list(
#     fun_name = "test_get_function",
#     fun = test_get_function,
#     args = list(
#       arg1 = list(1, 2),
#       comment = "clear",
#       1:3),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "list",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "character",
#       rows = NULL
#     ),
#     output = list(
#       cols = c("a", "b"),
#       class = "data.frame",
#       rows = 10L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
#
# test_that("unpack can unpack a mix of different ways to pass an argument in the same call", {
#   get_value <- function() {
#     TRUE
#   }
#   test_get_function <- function(data1, pos1, fun1, default1 = get_value(), ...) {
#     data.frame(a = c(1:10),
#                b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
#   }
#   quosure <- capture_quosure(test_get_function(data1 = list(1, 2), FALSE, fun1 = median, comment = "clear", data2 = 1:3))
#   list_unpacked_test <- unpack(quosure, link_type = "get")
#   list_unpack_expected <- list(
#     fun_name = "test_get_function",
#     fun = test_get_function,
#     args = list(
#       data1 = list(1, 2),
#       pos1 = FALSE,
#       fun1 = median,
#       default1 = TRUE,
#       comment = "clear",
#       data2 = 1:3),
#     package = "unkown", # During testthat the environment changes, otherwise .GlobalEnv
#     link_type = "get",
#     first = list(
#       cols = NULL,
#       class = "list",
#       rows = NULL
#     ),
#     second = list(
#       cols = NULL,
#       class = "logical",
#       rows = NULL
#     ),
#     output = list(
#       cols = c("a", "b"),
#       class = "data.frame",
#       rows = 10L
#     )
#   )
#   expect_identical(list_unpacked_test, list_unpack_expected)
# })
