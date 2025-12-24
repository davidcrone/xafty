test_that("A query that does not need merging, will be left unchanged", {
  expected_query <- query(proj1 = "col1", proj2 = "col2")
  test_query <- merge_queries(expected_query)
  expect_identical(test_query, expected_query)
})

test_that("Duplicated project in a query are merged together", {
  xafty_query <- query(proj1 = "col1", proj2 = "col2", proj1 = "col3")
  test_query <- merge_queries(xafty_query)
  expected_query <- query(proj1 = c("col1", "col3"), proj2 = "col2")
  expect_identical(test_query, expected_query)
})

test_that("Two seperate queries are merged into one query", {
  query_1 <- query(proj1 = "col1")
  query_2 <- query(proj2 = "col2")
  test_query <- merge_queries(query_1, query_2)
  expected_query <- query(proj1 = "col1", proj2 = "col2")
  expect_identical(test_query, expected_query)
})

test_that("Two seperate queries and duplicated projects are merged together", {
  query_1 <- query(proj1 = "col1")
  query_2 <- query(proj2 = "col2", proj1 = "col3")
  test_query <- merge_queries(query_1, query_2)
  expected_query <- query(proj1 = c("col1", "col3"), proj2 = "col2")
  expect_identical(test_query, expected_query)
})

test_that("Merging with an empty list returns a named empty xafty query", {
  test_query <- merge_queries(query(), list())
  expected_query <- query()
  expect_identical(test_query, expected_query)
})

test_that("merge_queries respects xafty_object_query class", {
  test_query <- merge_queries(query(project = "[object]"))
  expected_query <- query(project = "[object]")
  expect_identical(test_query, expected_query)
})

test_that("merge_queries can merge a query list which has been treated by fill_raw_query", {
  raw_query_list <- query("mean_nickname", customer_data = "id", "intelligence_plus_mean", map = "id")
  test_query <- fill_raw_query(query_list = raw_query_list, network = test_network)
  test_query <- merge_queries(test_query)
  exp_query <- query(customer_data = c("mean_nickname", "id"), intelligence = "intelligence_plus_mean", map = "id")
  expect_identical(test_query, exp_query)
})
