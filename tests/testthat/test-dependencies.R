test_that("A query with no dependencies returns the query", {
  expected_query <- query(customer_data = c("name"))
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = expected_query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query with a dependecy returns the query with the dependency", {
  query <- query(customer_data = "category")
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query from two layers returns the entire query", {
  query <- query(customer_data = c("category", "score"))
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies works with an empty list and returns a named empty query", {
  query <- list()
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list =  query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query()
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies from two projects can be retrieved and hidden dependencies revealed", {
  query_list <- query(customer_data = "category", occupations = "department")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query_list)
  dag_sm <- initialize_join_projects(query_list = globals$internal, network = test_network, dag_sm = dag_sm)
  sm <- resolve_dependencies(query_list = globals$internal, context_list = globals$context, state_list = globals$states, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name", "id"), occupations = c("department", "id"))
  expect_identical(sm$get_query(), expected_query)
})

