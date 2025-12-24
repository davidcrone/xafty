
test_that("project_needs_join correctly identifies a needed join for a single project", {
  query_list <- query(customer_data = "name")
  test_value <- project_needs_join(project = "customer_data", network = test_network, query_list = query_list)
  expect_identical(test_value, TRUE)
})

test_that("project_needs_join correctly identifies that a project needs a join ", {
  query_list <- query(customer_data = "name", occupations = "department")
  test_value <- vapply(c("customer_data", "occupations"), project_needs_join,
                       network = test_network, query_list = query_list, FUN.VALUE = logical(1))
  expect_identical(test_value, c("customer_data" = TRUE, "occupations" = TRUE))
})

test_that("A node from project derived from other projects does not need a join", {
  query_list <- query(customer_data = "score", intelligence = "new_column")
  test_value1 <- project_needs_join(project = "intelligence", network = test_network, query_list = query_list)
  test_value2 <- project_needs_join(project = "customer_data", network = test_network, query_list = query_list)
  expect_identical(test_value1, FALSE)
  expect_identical(test_value2, TRUE)
})

test_that("Combining a derived variable with a root node variable from the same project correctly identifies the need of a join", {
  query_list <- query(customer_data = "score", intelligence = c("new_column", "intelligence"))
  test_value1 <- project_needs_join(project = "intelligence", network = test_network, query_list = query_list)
  test_value2 <- project_needs_join(project = "customer_data", network = test_network, query_list = query_list)
  expect_identical(test_value1, TRUE)
  expect_identical(test_value2, TRUE)
})
