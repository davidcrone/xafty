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
