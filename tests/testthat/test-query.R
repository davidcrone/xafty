test_that("query correctly builds a standard xafty_query with 1 column", {
  xafty_query <- query(customer_data = "id")
  expect_equal(xafty_query$customer_data$select, "id")
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(names(xafty_query), "customer_data")
})

test_that("query correctly builds a standard xafty_query with 2 columns", {
  xafty_query <- query(customer_data = c("id", "score"))
  expect_equal(xafty_query$customer_data$select, c("id", "score"))
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(names(xafty_query), "customer_data")
})

test_that("query correctly builds a standard xafty_query with 2 projects", {
  xafty_query <- query(customer_data = c("id", "score"), occupations = "id")
  expect_equal(xafty_query$customer_data$select, c("id", "score"))
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(xafty_query$occupations$select, "id")
  expect_equal(xafty_query$occupations$from, "occupations")
  expect_equal(names(xafty_query), c("customer_data", "occupations"))
})

test_that("passing values as a list returns a standard xafty_query", {
  xafty_query <- query(list(customer_data = "id"))
  expect_equal(xafty_query$customer_data$select, "id")
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(names(xafty_query), "customer_data")
})


test_that("passing values as a list returns a standard xafty_query", {
  xafty_query <- query(list(customer_data = "id", occupations = "id"))
  expect_equal(xafty_query$customer_data$select, "id")
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(xafty_query$occupations$select, "id")
  expect_equal(xafty_query$occupations$from, "occupations")
  expect_equal(names(xafty_query), c("customer_data", "occupations"))
})

test_that("passing values as a list returns a standard xafty_query", {
  xafty_query <- query(list(customer_data = "id"), list(occupations = "id"))
  expect_equal(xafty_query$customer_data$select, "id")
  expect_equal(xafty_query$customer_data$from, "customer_data")
  expect_equal(xafty_query$occupations$select, "id")
  expect_equal(xafty_query$occupations$from, "occupations")
  expect_equal(names(xafty_query), c("customer_data", "occupations"))
})

test_that("passing just a character vecror into xafty_query returns the query with a star * select", {
  xafty_query <- query(customer_data = "*")
  expect_equal(xafty_query$customer_data$select, "*")
  expect_equal(xafty_query$customer_data$from, "customer_data")
})

test_that("an object query correctly adds the class xafty_object_query to the class vector", {
  xafty_query <- query(customer_data = "[my_object]")
  expect_in(c("xafty_object_query"), class(xafty_query))
})

test_that("trying to query an object and any other column or object gives an informative error", {
  expect_error(query(customer_data = c("[my_object]", "column")))
  expect_error(query(customer_data = "column", occupation = "[my_object]"))
})

test_that("adding a with list for states will return the query with the state list", {
  xafty_query <- query(customer_data = c("id", "score")) |>
    with(param1 = TRUE, param2 = FALSE)
  expect_equal(xafty_query$states$param1, expected = TRUE)
  expect_true(inherits(xafty_query$states, "xafty_states_list"))
})

test_that("interpolate_state_in_query correctly fills the select variable with state value from a state query", {
  network_env <- init_network("test_network")
  network_env$add_state("year", default = "2019")
  network_env$settings$state$global_default <- 2020

  state_query <- query(project1 = c("population.{year}", "overall_population{fromStateQuery}"), project2 = "default.{state}") |>
    with(fromStateQuery = 2023)
  query_test <- interpolate_state_in_query(query_list = state_query$query, state_list = state_query$states, network_env = network_env)
  query_expected <- query(project1 = c("population.2019", "overall_population2023"), project2 = "default.2020")
  expect_identical(query_test, query_expected)
})

test_that("interpolate_state_in_query correctly fills the select variable with the state value", {
  network_env <- init_network("test_network")
  network_env$add_state("year", default = "2019")
  network_env$settings$state$global_default <- 2020
  query_list <- query(project1 = c("population.{year}", "overall_population"), project2 = "default.{state}")
  query_test <- interpolate_state_in_query(query_list = query_list, state_list = NULL, network_env = network_env)
  query_expected <- query(project1 = c("population.2019", "overall_population"), project2 = "default.2020")
  expect_identical(query_test, query_expected)
})

test_that("interpolate_state_in_query correctly fills the select variable with the state value", {
  network_env <- init_network("test_network")
  network_env$add_state("year", default = "2019")
  query_list <- query(project1 = "population.{year}_test", project2 = "default{null}", project3 = "overall_population")
  query_test <- interpolate_state_in_query(query_list = query_list, state_list = NULL, network_env = network_env)
  query_expected <- query(project1 = "population.2019_test", project2 = "default", project3 = "overall_population")
  expect_identical(query_test, query_expected)
})

test_that("interpolate_state_in_query can also interpolate an object", {
  network_env <- init_network("test_network")
  network_env$add_state("year", default = "2019")
  query_list <- query(object = "[object.{year}]")
  query_test <- interpolate_state_in_query(query_list = query_list, state_list = NULL, network_env = network_env)
  query_expected <- query(object = "[object.2019]")
  expect_identical(query_test, query_expected)
})

test_that("a context component is added to the where part in the query", {
  test_query <- query(intelligence = "intelligence") |>
    where(intelligence = "active_customers")
  expect_equal(names(test_query), c("query", "context"))
  expect_equal(test_query$context$intelligence$select, "active_customers")
  expect_equal(test_query$context$intelligence$from, "intelligence")
  expect_s3_class(test_query$context$intelligence, class = c("context_query"))
})

test_that("characters with no name are created as a raw query", {
  test_query <- query("test")
  expected_query <- query(unevaluated = "test")
  class(expected_query$unevaluated) <- c("list", "raw_query")
  expect_identical(test_query, expected_query)
})

test_that("mixing named characters with unnamed characters yields a mix of raw_query and query", {
  test_query <- query("test", customer_data = "id")
  expected_query <- query(unevaluated = "test", customer_data = "id")
  class(expected_query$unevaluated) <- c("list", "raw_query")
  expect_identical(test_query, expected_query)
})

test_that("query with multiples of the same project presevers the variables", {
  xafty_query <- query(proj1 = "col1", proj2 = "col2", proj1 = "col3")
  tets_projects <- vapply(xafty_query, \(query) query$from, character(1), USE.NAMES = FALSE)
  tets_variables <- vapply(xafty_query, \(query) query$select, character(1), USE.NAMES = FALSE)
  expect_identical(tets_projects, c("proj1", "proj2", "proj1"))
  expect_identical(tets_variables, c("col1", "col2", "col3"))
})

test_that("A raw query is correctly filled with a project", {
  raw_query_list <- query("mean_nickname", customer_data = "id", "intelligence_plus_mean", map = "id")
  test_query <- fill_raw_query(query_list = raw_query_list, network = test_network)
  tets_projects <- vapply(test_query, \(query) query$from, character(1), USE.NAMES = FALSE)
  tets_variables <- vapply(test_query, \(query) query$select, character(1), USE.NAMES = FALSE)
  expect_identical(tets_projects, c("customer_data", "customer_data", "intelligence", "map"))
  expect_identical(tets_variables, c("mean_nickname", "id", "intelligence_plus_mean", "id"))
})
