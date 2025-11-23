test_that("Register builds a get node in the network environment correctly", {
    test_get_function <- function(arg1, arg2, ...) {
      data.frame(a = c(1:10),
                 b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
    }
    project_env <- init_network(name = "project_env")
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
  project_env <- init_network(name = "project_env")
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
  project_env <- init_network(name = "project_env")
  project_env$add_project("test")
  project_env$test$get(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3), vars = c("a", "b"))
  project_env$test$add(test_add_function(arg1 = test_get_function()))

  project_env$add_project("test2")
  project_env$test2$get(test2_get_function())
  project_env$test$join(test_join_function(data_left = query(test = "a"), data_right = query(test2 = "a")), vars = character(0))

  expect_equal(names(project_env$test$joined_projects), "test2")
  expect_equal(names(project_env$test2$joined_projects), "test")
  expect_equal(project_env$test2$joined_projects$test, "test_join_function")
  expect_equal(project_env$test$joined_projects$test2, "test_join_function")
})

test_that("register add also works with xafty link instead of passing data into the to be registered function", {
  test_state_1 <- init_network(name = "project_env")
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

test_that("A unjoined project's variable can be nascented from the network even if it is not joined", {
  test_state_1 <- init_network(name = "project_env")
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

test_that("A unjoined variable can be nascented from the network alongside traditional projects", {
  test_state_1 <- init_network(name = "project_env")
  test_state_1$add_project("customer_data")
  test_state_1$add_project("occupation")
  test_state_1$add_project("value_sheet")
  test_state_1$customer_data$get(get_sample_data())
  test_state_1$customer_data$add(add_score_category(data = query(customer_data = "score")))
  test_state_1$occupation$get(get_additional_info())
  test_state_1$customer_data$join(join_datasets(main_data = query(customer_data = "id"), extra_data = query(occupation = "id")))
  test_state_1$value_sheet$add(new_column_from_both_projects(query(occupation = "department", customer_data = "name")))
  test_state_1$value_sheet$add(add_column_to_intelligence(data = query(occupation = "department", customer_data = "id", value_sheet = "nickname")))
  table_test <- test_state_1 |> nascent(occupation = "department", value_sheet = "new_column")
  table_expected <- structure(list(
    department = c("HR", "IT", "Finance", "Marketing", "Sales"),
    new_column = c("HR1", "IT2", "Finance3", "Marketing4", "Sales5")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("register throws an error when a column within a query is not present within a project while the variable parameter is set", {
  test_func <- function(data) {
    data
  }
  expect_error(test_network$customer_data$add(test_func(data = query(customer_data = "col_not_present")), vars = c("add_col")))
})

test_that("register can register an object", {
  test_network <- init_network(name = "test_network", projects = "intelligence")
  test_network$intelligence$get(intelligence_date())
  filter_active_customers <- function(data) {
    data[data$intelligence > 100, ]
  }
  test_network$intelligence$add_object("active_customers", filter_active_customers(data = query(intelligence = "intelligence")))
  expect_equal(test_network$intelligence$variables$active_customers, "filter_active_customers")
  expect_equal(test_network$intelligence$ruleset$filter_active_customers$name, "active_customers")
})

test_that("register can register a function with an object as dependency", {
  test_network <- init_network(name = "test_network", projects = "intelligence")
  test_network$intelligence$get(intelligence_date())
  filter_active_customers <- function(data) {
    data[data$intelligence > 100, , drop = FALSE]
  }
  test_network$intelligence$add_object("active_customers", filter_active_customers(data = query(intelligence = "intelligence")))
  build_kpi <- function(active_customers) {
    mean(active_customers$intelligence)
  }
  test_network$intelligence$add_object("mean_intelligence", build_kpi(active_customers = query(intelligence = c("[active_customers]"))))
  expect_equal(test_network$intelligence$variables$mean_intelligence, "build_kpi")
  expect_equal(test_network$intelligence$ruleset$build_kpi$name, "mean_intelligence")
})

test_that("A xafty state can be registered", {
  test_network <- init_network(name = "test_network")
  test_network$add_state("test_state", allowed = c(TRUE, FALSE), example = TRUE, default = TRUE, documentation = "this is an example")
  expect_equal(get_default_state("test_state", test_network), TRUE)
  expect_equal(get_default_state("unregistered_state", test_network), NULL)
})

test_that("It is possible to register a variable with interpolated state, keeping the non interpolated query", {
  test_network <- init_network(name = "test_network", projects = "test_data")
  test_network$add_state("year", default = 2025)
  get_data <- function() {
    data.frame(
      data.2025 = c("A", "B"),
      data.2026 = c("C", "D")
    )
  }
  test_network$test_data$get(get_data())
  add_data <- function(data) {
    data$data.2027 <- c("E", "F")
    data
  }
  test_network$test_data$add(add_data(data = query(test_data = "data.{year}")))
  expect_equal(test_network$test_data$ruleset$add_data$variables, expected = "data.2027")
  expect_equal(test_network$test_data$ruleset$add_data$args$data, expected = query(test_data = "data.{year}"))
})

test_that("Updating a function with revised variable names removes all legacy variable names of that function", {
  network <- init_network("test", projects = "test_proj")
  network$test_proj$get(get_sample_data(), vars = c("i", "name", "score"))
  network$test_proj$get(get_sample_data(), vars = c("id", "name", "score"), update = TRUE)
  expect_in(names(network$test_proj$variables), c("id", "name", "score"))
})

test_that("Registering on_entry creates a on_entry function in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  remove_redundant_data <- function(data, values = FALSE) {
    data
  }
  test_network$customer_data$on_entry(name = "remove_redundant", fun = remove_redundant_data(data = "{.data}"))
  expect_identical(test_network$customer_data$wrappers$on_entry, "remove_redundant_data")
  expect_identical(test_network$customer_data$wrappers$on_exit, NULL)
  expect_identical(test_network$customer_data$ruleset$remove_redundant_data$args$data, "{.data}")
  expect_identical(test_network$customer_data$ruleset$remove_redundant_data$args$values, FALSE)
})

test_that("Registering on_entry creates a on_entry function in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  add_redundant_data <- function(data, values = FALSE) {
    data
  }
  test_network$customer_data$on_exit(name = "add_redundant", fun = add_redundant_data(data = "{.data}"))
  expect_identical(test_network$customer_data$wrappers$on_entry, NULL)
  expect_identical(test_network$customer_data$wrappers$on_exit, "add_redundant_data")
  expect_identical(test_network$customer_data$ruleset$add_redundant_data$args$data, "{.data}")
  expect_identical(test_network$customer_data$ruleset$add_redundant_data$args$values, FALSE)
})

test_that("Registering a on_entry with the same name twice, does not create a duplicate entries in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$on_entry(reorder_cars_by_color(cars = "test"), name = "reorder", update = TRUE)
  test_network$customer_data$on_entry(reorder_cars_by_color(cars = "test"), name = "reorder", update = TRUE)
  expect_length(test_network$customer_data$wrappers$on_entry, 1)
  expect_equal(test_network$customer_data$wrappers$on_entry, "reorder_cars_by_color")
})

test_that("Registering a on_entry with the same name twice, does not create a duplicate entries in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$on_exit(reorder_cars_by_color(cars = "test"), name = "reorder", update = TRUE)
  test_network$customer_data$on_exit(reorder_cars_by_color(cars = "test"), name = "reorder", update = TRUE)
  expect_length(test_network$customer_data$wrappers$on_exit, 1)
  expect_equal(test_network$customer_data$wrappers$on_exit, "reorder_cars_by_color")
})

# test_that("Registering context creates the correct entry in ruleset and network", {
#   test_network <- init_network(name = "test_network", projects = "intelligence")
#   test_network$intelligence$get(intelligence_date())
#   filter_active_customers <- function(data) {
#     data[data$intelligence > 100, , drop = FALSE]
#   }
#   test_network$intelligence$add_context("active_customers", filter_active_customers(data = query(intelligence = "intelligence")))
#   link <- test_network$intelligence$ruleset$filter_active_customers
#   expect_equal(link$fun_name, "filter_active_customers")
#   expect_equal(link$args$data, query(intelligence = "intelligence"))
#   expect_equal(link$name, "active_customers")
#   expect_equal(test_network$intelligence$variables$active_customers, "filter_active_customers")
# })
