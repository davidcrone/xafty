
test_that("where correctly disects an expression", {
  query_list <- query(test = "col1") |> where(test >= 1)
  query_test <- query_list[[2]]
  expr_test <- query_list[[3]]
  expect_identical(query_test$select, "test")
  expect_identical(expr_test$expr, quote(test >= 1))
  expect_s3_class(query_test, "raw_query")
})

test_that("objects are correctly left as objects", {
  query_list <- query(test = "[col1]") |> where(test >= 1)
  query_test <- query_list[[2]]
  expr_test <- query_list[[3]]
  expect_identical(query_test$select, "test")
  expect_identical(expr_test$expr, quote(test >= 1))
  expect_s3_class(query_list, "xafty_object_query")
})


test_that("where correctly disects an expression", {
  query_list <- query(test = "col1") |> where(score >= 10)
  filled_query <- fill_raw_query(query_list, test_network)
  query_test <- filled_query[[2]]
  expr_test <- filled_query[[3]]
  expect_identical(query_test$select, "score")
  expect_identical(query_test$from, "customer_data")
  expect_s3_class(query_test, "xafty_query")
  expect_s3_class(query_test, "where_query")
  expect_identical(expr_test$expr, quote(score >= 10))
})

test_that("where correctly disects an expression with two variables", {
  query_list <- query(test = "col1") |> where(score >= 10 & intelligence_plus_mean == 10)
  filled_query <- fill_raw_query(query_list, test_network)
  query_test1 <- filled_query[[2]]
  query_test2 <- filled_query[[3]]
  expr_test <- filled_query[[4]]
  expect_identical(query_test1$select, "score")
  expect_identical(query_test1$from, "customer_data")
  expect_identical(query_test2$select, "intelligence_plus_mean")
  expect_identical(query_test2$from, "intelligence")
  expect_s3_class(query_test1, c("where_query"))
  expect_s3_class(query_test2, c("where_query"))
  expect_identical(expr_test$expr, quote(score >= 10 & intelligence_plus_mean == 10))
})

