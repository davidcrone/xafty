test_that("Scoped columns are correctly retrieved from a query", {
  xafty_query <- query(customer_data = "col1", occupations = c("col2", "col3"), customer_data = "col4")
  scoped_column_order <- get_scoped_column_order(query = xafty_query)
  expect_identical(scoped_column_order, c("customer_data.col1", "occupations.col2", "occupations.col3", "customer_data.col4"))
})

test_that("Project order returns the correct amount of projects", {
  xafty_query <- query(customer_data = "col1", occupations = c("col2", "col3"), customer_data = "col4")
  project_order <- get_project_order(query = xafty_query)
  expect_identical(project_order, c("customer_data", "occupations", "occupations", "customer_data"))
})

test_that("Column names are returned in correct order", {
  xafty_query <- query(customer_data = "col1", occupations = c("col2", "col3"), customer_data = "col4")
  column_order <- get_column_order(query = xafty_query)
  expect_identical(column_order, c("col1", "col2", "col3", "col4"))
})

test_that("Scoped Columns works with empty list and return an empty list", {
  scoped_column_order <- get_scoped_column_order(query = list())
  expect_identical(scoped_column_order, character(0))
})

test_that("Project order returns the correct amount of projects", {
  project_order <- get_project_order(query = list())
  expect_identical(project_order, character(0))
})

test_that("Column names are returned in correct order", {
  column_order <- get_column_order(query = list())
  expect_identical(column_order, character(0))
})
