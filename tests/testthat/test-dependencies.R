test_that("A query with no dependencies returns the query", {
  expected_query <- query(customer_data = c("name"))
  test_query <- dependencies(query_list = expected_query, network = test_network)
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query with a dependecy returns the query with the dependency", {
  expected_query <- query(customer_data = "category")
  test_query <- dependencies(query = expected_query, network = test_network)
})
