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
