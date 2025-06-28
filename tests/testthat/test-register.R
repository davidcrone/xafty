test_that("Register builds a get node in the network environment correctly", {
    test_get_function <- function(arg1, arg2, ...) {
      data.frame(a = c(1:10),
                 b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
    }
    project_env <- init_network()
    project_env$add_project("test")
    project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
    expect_equal(names(project_env$test$variables), c("a", "b"))
    expect_equal(project_env$test$variables$a, "test_get_function")
    expect_equal(project_env$test$variables$b, "test_get_function")
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
  project_env$add_project("test")
  project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
  project_env$test$add(test_add_function(arg1 = test_get_function()))
  expect_equal(names(project_env$test$variables), c("a", "b", "c"))
  expect_equal(project_env$test$variables$c, "test_add_function")
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
  project_env$add_project("test")
  project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
  project_env$test$add(test_add_function(arg1 = test_get_function()))

  project_env$add_project("test2")
  project_env$test2$get(test2_get_function())
  project_env$test$join(test_join_function(data_left = query(test = "a"), data_right = query(test2 = "a")))

  expect_equal(names(project_env$test$joined_projects), "test2")
  expect_equal(names(project_env$test2$joined_projects), "test")
  expect_equal(project_env$test2$joined_projects$test, "test_join_function")
  expect_equal(project_env$test$joined_projects$test2, "test_join_function")
})

test_that("register add also works with xafty link instead of passing data into the to be registered function", {
  test_state_1 <- init_network()
  test_state_1$add_project("customer_data")
  test_state_1$customer_data$get(get_sample_data())
  xafty_link <- query(customer_data = c("score", "name"))
  test_state_1$customer_data$add(add_score_category(data = xafty_link))
  xafty_test_pull <- query(customer_data = c("name", "category"))
  data_test <- test_state_1 |> nascent(xafty_test_pull)
  data_expected <- structure(row.names = c(NA, -5L), class = "data.frame",
    list(
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    category = c("Low", "High", "Low", "High", "Low")
    ))
  expect_equal(data_test, data_expected)
})

test_that("New environment type xafty_bundle allows to bundle queries with as sub projects", {
  test_state_1 <- init_network()
  test_state_1$add_project("customer_data")
  test_state_1$add_project("occupation")
  test_state_1$customer_data$get(get_sample_data())
  test_state_1$customer_data$add(add_score_category(data = query(customer_data = "score")))
  test_state_1$occupation$get(get_additional_info())
  test_state_1$customer_data$join(join_datasets(main_data = query(customer_data = "id"), extra_data = query(occupation = "id")))
  xafty_query <- query(customer_data = c("name", "category"), occupation = "department")
  test_state_1$add_project("value_sheet", xafty_query)
  data_test <- test_state_1$value_sheet$entry("data")
  data_expected <- structure(row.names = c(NA, -5L), class = "data.frame",
   list(
     name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
     category = c("Low", "High", "Low", "High", "Low"),
     department = c("HR", "IT", "Finance", "Marketing", "Sales")
   ))
  expect_identical(data_test, data_expected)
})

test_that("xafty_bundle allows to seamlessly add a column to the project", {
  test_state_1 <- init_network()
  test_state_1$add_project("customer_data")
  test_state_1$add_project("occupation")
  test_state_1$add_project("value_sheet")
  test_state_1$customer_data$get(get_sample_data())
  test_state_1$customer_data$add(add_score_category(data = query(customer_data = "score")))
  test_state_1$occupation$get(get_additional_info())
  test_state_1$customer_data$join(join_datasets(main_data = query(customer_data = "id"), extra_data = query(occupation = "id")))
  test_state_1$value_sheet$add(new_column_from_both_projects(query(occupation = "department", customer_data = "name")))
  table_test <- test_state_1 |> nascent(value_sheet = "nickname")
  table_expected <- structure(list(nickname = c("HRAlice", "ITBob", "FinanceCharlie",
                       "MarketingDiana", "SalesEve")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

