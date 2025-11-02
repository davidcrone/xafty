
test_that("get_xafty_objects_vec returns an empty vector when no args are present", {
  test_link <- test_create_link(get_sample_data())
  test_vec <- get_xafty_objects_vec(test_link)
  expected_vec <- character(0)
  expect_identical(test_vec, expected_vec)
})

test_that("get_xafty_objects_vec returns 'xafty_query' when a xafty_query is passed as an argument", {
  test_link <- test_create_link(add_score_category(query(customer_data = c("score", "name"))))
  test_vec <- get_xafty_objects_vec(test_link)
  expected_vec <- c(data = "xafty_query")
  expect_identical(test_vec, expected_vec)
})

test_that("get_xafty_objects_vec returns 'xafty_query' of length 2 when a xafty_query is passed in two arguments", {
  test_link <- test_create_link(join_datasets(main_data = query(customer_data = c("id", "category")), extra_data = query(occupations = "id")))
  test_vec <- get_xafty_objects_vec(test_link)
  expected_vec <- c(main_data = "xafty_query", extra_data = "xafty_query")
  expect_identical(test_vec, expected_vec)
})

test_that("get_xafty_objects_vec returns 'none_xafty_object' when neither a query nor a state was passed into the arg", {
  test_link <- test_create_link(join_datasets(main_data = TRUE, extra_data = query(occupations = "id")), vars = character(0))
  test_vec <- get_xafty_objects_vec(test_link)
  expected_vec <- c(main_data = "none_xafty_object", extra_data = "xafty_query")
  expect_identical(test_vec, expected_vec)
})

test_that("get_xafty_objects_vec returns 'xafty_state' when a braced character vector was passed to the argument", {
  test_link <- test_create_link(join_datasets(main_data = "{xafty_state}", extra_data = query(occupations = "id")), vars = character(0))
  test_vec <- get_xafty_objects_vec(test_link)
  expected_vec <- c(main_data = "xafty_state", extra_data = "xafty_query")
  expect_identical(test_vec, expected_vec)
})

test_that("get_queries returns an empty list when no queries are present in args", {
  test_link <- test_create_link(get_sample_data())
  test_query <- get_queries(test_link)
  expected_query <- list()
  expect_identical(test_query, expected_query)
})

test_that("get_queries returns an empty list of all argumentsa are non_xafty_objects", {
  test_link <- test_create_link(join_datasets(main_data = FALSE, extra_data = NULL), vars = character(0))
  test_query <- get_queries(test_link)
  expected_query <- list()
  expect_identical(test_query, expected_query)
})

test_that("get_queries returns the query of a single argument", {
  test_link <- test_create_link(add_score_category(query(customer_data = c("score", "name"), occupations = "department")), vars = character(0))
  test_query <- get_queries(test_link)
  expected_query <- list(data = query(customer_data = c("score", "name") , occupations = "department"))
  expect_identical(test_query, expected_query)
})

test_that("get_queries ignores a xafty state and returns the query from the second argument", {
  test_link <- test_create_link(join_datasets(main_data = "{xafty_state}", extra_data = query(occupations = "id")), vars = character(0))
  test_query <- get_queries(test_link)
  expected_query <- list(extra_data = query(occupations = "id"))
  expect_identical(test_query, expected_query)
})

test_that("an object can be build as a correct link", {
  filter_active_customers <- function(customer_data) {
    customer_data[customer_data$intelligence > 100, ]
  }
  test_link <- test_create_link(filter_active_customers(customer_data = query(customer_data = c("id", "name"),
                                 intelligence = "intelligence")), name = "active_customers", link_type = "object")
  expect_equal(test_link$name, "active_customers")
})

test_that("get_join_dependencies returns the dependend joins of the link", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("score", "name"), occupations = "department")),
                           project = "customer_data", vars = character(0))
  expect_in(link$joins$projects$data, c("customer_data", "occupations"))
})

test_that("get_join_dependencies returns an empty list, when no joins are found", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("score", "name"))), project = "customer_data",  vars = character(0))
  expect_equal(link$joins$projects, setNames(list(), character(0)))
})

test_that("get_join_dependencies returns an empty list when no depndend queries are found", {
  link <- test_create_link(add_score_category(data = TRUE), vars = character(0))
  expect_equal(link$joins$projects, list())
})

test_that("get_join_dependencies does not mistake the same project present twice in a query for a join", {
  link <- test_create_link(add_score_category(data = query(customer_data = "score", customer_data = "name")), project = "customer_data",  vars = character(0))
  expect_equal(link$joins$projects, setNames(list(), character(0)))
})

test_that("get_join_dependencies returns the dependend joins of the link", {
  link <- test_create_link(add_score_category(data = query(customer_data = c("score", "name"), occupations = "department")),
                           project = "unjoined_project", vars = character(0))
  expect_in(link$joins$projects$data, c("customer_data", "occupations"))
})

