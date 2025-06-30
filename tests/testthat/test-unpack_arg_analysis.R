test_that("unpack dependencies resolves the dependencies correctly when no argument is given", {
  test_function <- function() {
    data.frame(a = c("1", "s"))
  }
  list_depends_test <- test_arg_dependencies(test_function())
  list_depends_expected <- list()
  expect_identical(list_depends_test, list_depends_expected)
})

test_that("unpack dependencies resolves the dependencies correctly when an unrelated argument is given", {
  test_function <- function(arg) {
    data.frame(a = c("1", "s"))
  }
  list_depends_test <- test_arg_dependencies(test_function(arg = TRUE))
  list_unpack_expected <- list_depends_expected <- setNames(list(), character(0)) # Create empty named list
  expect_identical(list_depends_test, list_depends_expected)
})

test_that("unpack dependencies works with a single project passed to the first argument", {
  list_depends_test <- test_arg_dependencies(add_score_category(data = query(customer_data = c("score", "name"))))
  list_depends_expected <- list(
    data = list(
      lead = "customer_data",
      cols = list(
        customer_data = list(
          select = c("score", "name"),
          funs = c("get_sample_data")
        )
      ),
      joins = character(0)
    )
  )
  expect_identical(list_depends_test, list_depends_expected)
})

test_that("unpack dependencies works with a two projects passed to the first and second argument respectively", {
  list_depends_test <- test_arg_dependencies(join_datasets(main_data = query(customer_data = c("id", "category")), extra_data = query(occupations = "id")))
  list_depends_expected <- list(
    main_data = list(
      lead = "customer_data",
      cols = list(
        customer_data = list(
          select = c("id", "category"),
          funs = c("get_sample_data", "add_score_category")
        )
      ),
      joins = character(0)
    ),
    extra_data = list(
      lead = "occupations",
      cols = list(
        occupations = list(
          select = c("id"),
          funs = c("get_additional_info")
        )
      ),
      joins = character(0)
    )
  )
  expect_identical(list_depends_test, list_depends_expected)
})

test_that("unpack dependencies works with a two projects passed both to the first", {
  list_depends_test <- test_arg_dependencies(new_column_from_both_projects(data = query(customer_data = "name", occupations = "department")))
  list_depends_expected <- list(
    data = list(
      lead = "customer_data",
      cols = list(
        customer_data = list(
          select = c("name"),
          funs = c("get_sample_data")
        ),
        occupations = list(
          select = "department",
          funs = "get_additional_info"
        )
      ),
      joins = c("customer_data", "occupations")
    )
  )
  expect_identical(list_depends_test, list_depends_expected)
})
