test_that("Register builds a get node in the network environment correctly", {
    test_get_function <- function(arg1, arg2, ...) {
      data.frame(a = c(1:10),
                 b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
    }
    project_env <- init_network(name = "project_env")
    project_env$add_project("test")
    project_env$test$link(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
    expect_equal(names(get_all_variables(project = "test", network = project_env)), c("a", "b"))
    expect_equal(get_all_variables(project = "test", network = project_env)[["a"]], "test_get_function")
    expect_equal(get_all_variables(project = "test", network = project_env)[["b"]], "test_get_function")
})

test_that("Register builds a add node in the network environment correctly", {
  test_get_function <- function(arg1, arg2, ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  test_add_function <- function(arg1, arg2) {
    arg1$c <- rep(c("value1", "value2"), 5, each = TRUE)
    arg1
  }
  project_env <- init_network(name = "project_env")
  project_env$add_project("test")
  project_env$test$link(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3))
  project_env$test$link(test_add_function(arg1 = test_get_function(), arg2 = query(test = c("a", "b"))), update = TRUE)
  expect_equal(names(get_all_variables(project = "test", network = project_env)), c("a", "b", "c"))
  expect_equal(get_all_variables(project = "test", network = project_env)[["c"]], "test_add_function")
})

test_that("Register builds a join node in the network environment correctly", {
  test_get_function <- function(arg1, arg2, ...) {
    data.frame(a = c(1:10),
               b = sample(c("1", "2", "3"), size = 10, replace = TRUE))
  }
  test_add_function <- function(arg1) {
    arg1$c <- rep(c("value1", "value2"), 5, each = TRUE)
    arg1
  }
  test2_get_function <- function() {
    data.frame(a = c(1, 2, 3),
               d = c("g", "h", "g"))
  }
  test_join_function <- function(data_left, data_right) {
    base::merge(data_left, data_right, by = intersect(names(data_left), names(data_right)), all.x = TRUE, sort = FALSE)
  }
  project_env <- init_network(name = "project_env")
  project_env$add_project("test")
  project_env$test$link(test_get_function(arg1 = list(1, 2), arg2 = TRUE, comment = "clear", 1:3), vars = c("a", "b"))
  project_env$test$link(test_add_function(arg1 = query(test = c("a", "b"))), update = TRUE)

  project_env$add_project("test2")
  project_env$test2$link(test2_get_function())
  project_env$test$link(test_join_function(data_left = query(test = "a"), data_right = query(test2 = "a")), vars = character(0), direction = "both")

  expect_equal(names(project_env$test$ruleset$nodes$joins), "test2")
  expect_equal(names(project_env$test2$ruleset$nodes$joins), "test")
  expect_equal(project_env$test$ruleset$nodes$joins$test2$link$fun_name, "test_join_function")
  expect_equal(project_env$test2$ruleset$nodes$joins$test$link$fun_name, "test_join_function")
})

test_that("One-directional joins (default) correctly only register the join in the indexed project", {
  network <- init_network(name = "project_env", projects = c("test", "test2"))
  network$test$link(get_sample_data())
  network$test2$link(get_additional_info())
  network$test$link(join_datasets(main_data = query(test = c("id")), extra_data = query(test2 = "id")))
  expect_equal(names(network$test$ruleset$nodes$joins), "test2")
  expect_equal(names(network$test2$ruleset$nodes$joins), NULL)
  expect_equal(network$test2$ruleset$nodes$joins$test$link$fun_name,  NULL)
  expect_equal(network$test$ruleset$nodes$joins$test2$link$fun_name, "join_datasets")
})

test_that("register add also works with xafty link instead of passing data into the to be registered function", {
  test_state_1 <- init_network(name = "project_env")
  test_state_1$add_project("customer_data")
  test_state_1$customer_data$link(get_sample_data())
  xafty_link <- query(customer_data = c("score", "name"))
  test_state_1$customer_data$link(add_score_category(data = xafty_link))
  xafty_test_pull <- query(customer_data = c("name", "category"))
  data_test <- xafty_test_pull |> nascent(test_state_1)
  data_expected <- structure(row.names = c(NA, -5L), class = "data.frame",
    list(
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    category = c("Low", "High", "Low", "High", "Low")
    ))
  expect_equal(data_test, data_expected)
})

test_that("register throws an error when a column within a query is not present within a project while the variable parameter is set", {
  test_func <- function(data) {
    data
  }
  expect_error(test_network$customer_data$add(test_func(data = query(customer_data = "col_not_present")), vars = c("add_col")))
})

test_that("A xafty state can be registered", {
  test_network <- init_network(name = "test_network")
  test_network$add_state("test_state", allowed = c(TRUE, FALSE), example = TRUE, default = TRUE, documentation = "this is an example")
  expect_equal(get_default_state("test_state", test_network), TRUE)
  expect_equal(get_default_state("unregistered_state", test_network), NULL)
})

test_that("It is possible to register a variable with interpolated state, keeping the non interpolated query", {
  test_network <- init_network(name = "test_network", projects = "test_data")
  test_network$add_state("year", default = 2025)
  get_data <- function() {
    data.frame(
      data.2025 = c("A", "B"),
      data.2026 = c("C", "D")
    )
  }
  test_network$test_data$link(get_data())
  add_data <- function(data) {
    data$data.2027 <- c("E", "F")
    data
  }
  test_network$test_data$link(add_data(data = query(test_data = "data.{year}")))
  expect_equal(test_network$test_data$ruleset$nodes$links$add_data$variables, expected = "data.2027")
  expect_equal(test_network$test_data$ruleset$nodes$links$add_data$args$data, expected = query(test_data = "data.{year}"))
})

test_that("Updating a function with revised variable names removes all legacy variable names of that function", {
  network <- init_network("test", projects = "test_proj")
  network$test_proj$link(get_sample_data(), vars = c("i", "name", "score"))
  network$test_proj$link(get_sample_data(), vars = c("id", "name", "score"), update = TRUE)
  expect_in(names(get_all_variables(project = "test_proj", network = network)), c("id", "name", "score"))
})

test_that("Registering on_entry creates a on_entry function in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  remove_redundant_data <- function(data, values = FALSE) {
    data
  }
  test_network$customer_data$add_context("group", on_entry = remove_redundant_data(data = "{.data}"))
  expect_identical(names(test_network$customer_data$ruleset$contexts$group$on_entry), "remove_redundant_data")
  expect_identical(names(test_network$customer_data$ruleset$contexts$group$on_exit), NULL)
  expect_identical(test_network$customer_data$ruleset$contexts$group$on_entry$remove_redundant_data$link$args$data, "{.data}")
  expect_identical(test_network$customer_data$ruleset$contexts$group$on_entry$remove_redundant_data$link$args$values, FALSE)
})

test_that("Registering on_entry creates a on_entry function in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  add_redundant_data <- function(data, values = FALSE) {
    data
  }
  test_network$customer_data$add_context(name = "group", on_exit = add_redundant_data(data = "{.data}"))
  expect_identical(names(test_network$customer_data$ruleset$contexts$group$on_entry), NULL)
  expect_identical(names(test_network$customer_data$ruleset$contexts$group$on_exit), "add_redundant_data")
  expect_identical(test_network$customer_data$ruleset$contexts$group$on_exit$add_redundant_data$link$args$data, "{.data}")
  expect_identical(test_network$customer_data$ruleset$contexts$group$on_exit$add_redundant_data$link$args$values, FALSE)
})

test_that("Registering a on_entry with the same name twice, does not create a duplicate entries in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$add_context("group", on_entry = reorder_cars_by_color(cars = "test"))
  test_network$customer_data$add_context("group", on_entry = reorder_cars_by_color(cars = "test"), update = TRUE)
  expect_length(test_network$customer_data$ruleset$contexts$group$on_entry, 1)
  expect_equal(names(test_network$customer_data$ruleset$contexts$group$on_entry), "reorder_cars_by_color")
})

test_that("Registering a on_exit with the same name twice, does not create a duplicate entries in wrappers", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$add_context("group", on_exit = reorder_cars_by_color(cars = "test"))
  test_network$customer_data$add_context("group", on_exit = reorder_cars_by_color(cars = "test"), update = TRUE)
  expect_length(test_network$customer_data$ruleset$contexts$group$on_exit, 1)
  expect_equal(names(test_network$customer_data$ruleset$contexts$group$on_exit), "reorder_cars_by_color")
})

test_that("Registering a link with a group adds the group value correctly to the link list", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$add_group("test_group")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "test_group")
  expect_identical(test_network$customer_data$ruleset$groups$test_group$variables, "category")
  expect_identical(names(test_network$customer_data$ruleset$groups), "test_group")
})

test_that("Updating a link with a group removed correctly deletes the variables from the group entry", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$add_group("test_group")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "test_group")
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), update = TRUE, group = NULL)
  expect_identical(test_network$customer_data$groups$test_group$variables, NULL)
  expect_identical(test_network$customer_data$ruleset$add_score_category$group, NULL)
})

test_that("Registering a link with a group that doesn't exist auto-creates the group", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  # Don't call add_group - let it be auto-created
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "auto_group", update = TRUE)
  expect_identical(test_network$customer_data$ruleset$groups$auto_group$variables, "category")
  expect_identical(names(test_network$customer_data$ruleset$groups), "auto_group")
})

test_that("Multiple links can be added to an auto-created group", {
  add_score_category2 <- add_score_category
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "scores", update = TRUE)
  # Add another link to the same auto-created group
  test_network$customer_data$link(add_score_category2(data = query(customer_data = c("score", "name"))), vars = "category2", group = "scores", update = TRUE)
  expect_equal(test_network$customer_data$ruleset$groups$scores$variables, c("category", "category2"))
})

test_that("Adding a polluted on_exit context node informs the user with a warning", {
  add_score_category2 <- add_score_category
  pass_through2 <- pass_through
  test_network <- init_network("test_network", projects = c("customer_data"))
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$add_context("polluted", on_entry = pass_through(data = "{.data}"))
  test_network$customer_data$link(add_score_category2(data = query(customer_data = "score")), vars = "category2", attach_context = "polluted")
  test_network$customer_data$link(add_score_category(data = query(customer_data = "category2")), vars = "category")

  expect_warning(test_network$customer_data$add_context("polluted", on_entry = pass_through(data = "{.data}"),
                                                                    on_exit = pass_through2(data = query(customer_data = "category")), update = TRUE, test_dag = TRUE)
                 )
})

test_that("Polluting a context upon updating a dependency throws a warning", {
  skip("Not implemented yet!")
  add_score_category2 <- add_score_category
  pass_through2 <- pass_through
  test_network <- init_network("test_network", projects = c("customer_data"))
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")), vars = "category")
  test_network$customer_data$add_context("polluted", on_entry = pass_through(data = "{.data}"), on_exit = pass_through2(data = query(customer_data = "category")))
  test_network$customer_data$link(add_score_category2(data = query(customer_data = "score")), vars = "category2", attach_context = "polluted")
  expect_warning(
    test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "category2"))), vars = "category", update = TRUE)
  )
})

test_that("Register raises an error when a node is registered with context that has not yet been added to the network", {
  on_entry_network <- init_network("on_entry", projects = c("cars"))
  on_entry_network$cars$link(test_get_car_data(conn = TRUE))
  expect_warning(on_entry_network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))), attach_context = "group"))
})

test_that("Registering the same variable name in the same project through different nodes, raises an error", {
  test_add_car_color2 <- test_add_car_color
  network <- init_network("network", projects = "cars")
  network$cars$link(test_get_car_data(conn = TRUE))
  network$cars$link(test_add_car_color(data = query(cars = "Car")))
  expect_error(network$cars$link(test_add_car_color2(data = query(cars = "Car"))))
  expect_no_error(network$cars$link(test_get_car_data(conn = TRUE), update = TRUE))
})

test_that("Renaming a variable in query does work seamlessly in register", {
  add_score_category <- function(data) {
    data$category <- ifelse(data$points >= 90, "High", "Low")
    data
  }
  test_network <- init_network("test_network")
  test_network$add_project("customer_data", info = "Customer Names and ID")
  test_network$customer_data$link(get_sample_data(), group = NULL)
  test_network$customer_data$link(add_score_category(data = query(customer_data = c(c("points" = "score"), "name"))), group = "test")
  expect_equal(test_network$customer_data$ruleset$nodes$links$add_score_category$variables, "category")
})

test_that("Register detects when the user creates a cyclic dependency", {
  exp_link <- test_network$customer_data$ruleset$nodes$links$new_column_from_both_projects
  expect_error(test_network$customer_data$link(new_column_from_both_projects(
    query(customer_data = c("mean_nickname", "name"), occupations = "department")),
    vars = "nickname", update = TRUE))
  test_link <- test_network$customer_data$ruleset$nodes$links$add_new_nickname$args$data
  expect_identical(test_link, exp_link)
})
