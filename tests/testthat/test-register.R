test_that("Register builds a get node in the network environment correctly", {
    test_get_function <- function(arg1, arg2, ...) {
      data.frame(a = c(1:10),
                 b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
    }
    project_env <- init_network()
    project_env$new_project("test")
    project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
    expect_equal(names(project_env$test$variables), c("a", "b"))
    expect_equal(project_env$test$variables$a, list(fun_name = "test_get_function", pull = NULL, from = NULL, exec = NULL, push = c("a", "b"), into = "test", joins = NULL))
    expect_equal(project_env$test$variables$b, list(fun_name = "test_get_function", pull = NULL, from = NULL, exec = NULL, push = c("a", "b"), into = "test", joins = NULL))
})

test_that("Register builds a add node in the network environment correctly", {
  test_get_function <- function(arg1, arg2, ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  test_add_function <- function(arg1) {
    arg1$c <- rep(c("value1", "value2"), 5, each = TRUE)
    arg1
  }
  project_env <- init_network()
  project_env$new_project("test")
  project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
  project_env$test$add(test_add_function(arg1 = test_get_function()))
  expect_equal(names(project_env$test$variables), c("a", "b", "c"))
  expect_equal(project_env$test$variables$c$fun_name, "test_add_function")
  expect_equal(project_env$test$variables$c$pull, c("a", "b"))
  expect_equal(project_env$test$variables$c$from, c("test", "test"))
  expect_equal(project_env$test$variables$c$exec, c("test_get_function", "test_get_function"))
  expect_equal(project_env$test$variables$c$push, "c")
  expect_equal(project_env$test$variables$c$into, "test")
  expect_equal(project_env$test$variables$c$joins, NULL)
})

test_that("Register builds a join node in the network environment correctly", {
  test_get_function <- function(arg1, arg2, ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  test_add_function <- function(arg1) {
    arg1$c <- rep(c("value1", "value2"), 5, each = TRUE)
    arg1
  }
  test2_get_function <- function() {
    data.frame(a = c(1, 2, 3),
               d = c("g", "h", "g"))
  }
  test_join_function <- function(data_left, data_right) {
    base::merge(data_left, data_right, by = intersect(names(data_left), names(data_right)), all.x = TRUE, sort = FALSE)
  }
  project_env <- init_network()
  project_env$new_project("test")
  project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
  project_env$test$add(test_add_function(arg1 = test_get_function()))

  project_env$new_project("test2")
  project_env$test2$get(test2_get_function())
  project_env$test$join(with = "test2", test_join_function(data_left = test_get_function()["a"], data_right = test2_get_function()))

  expect_equal(names(project_env$test$joined_projects), "test2")
  expect_equal(names(project_env$test2$joined_projects), "test")
  expect_equal(project_env$test2$joined_projects$test$fun_name, "test_join_function")
  expect_equal(project_env$test$joined_projects$test2$fun_name, "test_join_function")
})

test_that("register add also works with xafty link instead of passing data into the to be registered function", {
  test_state_1 <- init_network()
  test_state_1$new_project("customer_data")
  test_state_1$customer_data$get(get_sample_data())
  xafty_link <- pull_link(customer_data = c("score", "name"))
  test_state_1$customer_data$add(add_score_category(data = xafty_link))
  xafty_test_pull <- pull_link(customer_data = c("name", "category"))
  data_test <- test_state_1 |> nascent(xafty_test_pull)
  data_expected <- structure(row.names = c(NA, -5L), class = "data.frame",
    list(
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    category = c("Low", "High", "Low", "High", "Low")
    ))
  expect_equal(data_test, data_expected)
})
