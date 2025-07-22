test_that("A query with no dependencies returns the query", {
  expected_query <- query(customer_data = c("name"))
  test_query <- dependencies(query_list = expected_query, network = test_network)
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query with a dependecy returns the query with the dependency", {
  query <- query(customer_data = "category")
  test_query <- dependencies(query = query, network = test_network)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query from two layers returns the entire query", {
  query <- query(customer_data = c("category", "score"))
  test_query <- dependencies(query = query, network = test_network)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies works with an empty list and returns a named empty query", {
  query <- list()
  test_query <- dependencies(query = query, network = test_network)
  expected_query <- query()
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies from two projects can be retrieved and hidden dependencies revieled", {
    query <- query(customer_data = "category", occupations = "department")
    test_query <- resolve_dependencies(xafty_list = query, network = test_network, sm = build_tree())
    expected_query <- query(customer_data = c("category", "score", "name", "id"), occupations = c("department", "id"))
    expect_identical(test_query$get_query(), expected_query)
})
