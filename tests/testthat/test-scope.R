test_that("scope correctly scopes columns with the project name", {
  link <- test_create_link(get_sample_data(), project = "customer_data", network = test_network)
  data <- data.frame("id" = integer(), "score" = character(), "name" = character())
  data <- scope(data = data, link = link, mask = NULL)
  scoped_cols <- colnames(data)
  expect_equal(scoped_cols, c("customer_data.id", "customer_data.score", "customer_data.name"))
})

test_that("unscope correctly unscopes column names removing the project", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("id", "score"))),
                           project = "customer_data", network = test_network)
  data <- data.frame("customer_data.id" = integer(), "customer_data.score" = character(), "customer_data.name" = character())
  data <- unscope(data = data, link = link, arg_name = "data", mask = NULL)
  unscoped_cols <- colnames(data)
  expect_equal(unscoped_cols, c("id", "score", "customer_data.name"))
})

test_that("the action of unscop is reversed by scope", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("id", "score"))),
                           project = "customer_data", network = test_network, vars = character(0))
  data <- data.frame("customer_data.id" = integer(), "customer_data.score" = character(), "customer_data.name" = character())
  data <- unscope(data = data, link = link, arg_name = "data", mask = NULL)
  data <- scope(data = data, link = link, mask = NULL)
  scoped_cols <- colnames(data)
  expect_equal(scoped_cols, c("customer_data.id", "customer_data.score", "customer_data.name"))
})

test_that("masked columns are scoped even if the project name was changed", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("id", "score"))),
                           project = "customer_data", network = test_network)
  mask <- list(id = c("map", "customer_data"))
  data <- data.frame("map.id" = integer(), "customer_data.score" = character(), "customer_data.name" = character())
  data <- unscope(data = data, link = link, arg_name = "data", mask = mask)
  unscoped_cols <- colnames(data)
  expect_equal(unscoped_cols, c("id", "score", "customer_data.name"))
})

test_that("scope correctly scopes masked columns with the first project", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("id", "score"))),
                           project = "customer_data", network = test_network, vars = character(0))
  mask <- list(id = c("map", "customer_data"))
  data <- data.frame("id" = integer(), "score" = character(), "customer_data.name" = character())
  data <- scope(data = data, link = link, mask = mask)
  unscoped_cols <- colnames(data)
  expect_equal(unscoped_cols, c("map.id", "customer_data.score", "customer_data.name"))
})
