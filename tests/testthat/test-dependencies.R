test_that("A query with no dependencies returns the query", {
  expected_query <- query(customer_data = c("name"))
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = expected_query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query with a dependecy returns the query with the dependency", {
  query <- query(customer_data = "category")
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("Query from two layers returns the entire query", {
  query <- query(customer_data = c("category", "score"))
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list = query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name"))
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies works with an empty list and returns a named empty query", {
  query <- list()
  dag_sm <- build_tree(test_network)
  test_query <- dependencies(query_list =  query, state_list = NULL, network = test_network, dag_sm = dag_sm)
  expected_query <- query()
  expect_identical(test_query$get_query(), expected_query)
})

test_that("dependencies from two projects can be retrieved and hidden dependencies revealed", {
  query_list <- query(customer_data = "category", occupations = "department")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query_list)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = test_network, dag_sm = dag_sm)
  expected_query <- query(customer_data = c("category", "score", "name", "id"), occupations = c("department", "id"))
  expect_identical(sm$get_query(), expected_query)
})

test_that("on entry takes copies foreign dependency of nodes", {
  network <- init_network("test", projects = c("cars", "group"))
  network$cars$get(get_sample_data())
  network$group$on_entry(pass_through(data = "{.data}"))
  network$group$add(add_score_category(data = query(cars = "score")))
  dag <- build_dag(query(group = "category"), network = network)
  test_deps <- dag$dag[["group.pass_through"]]
  expect_identical(test_deps, "cars.get_sample_data")
})

test_that("on exit takes the dependency from the wrapper nodes", {
  network <- init_network("test", projects = c("cars", "group"))
  network$cars$get(get_sample_data())
  network$group$on_exit(pass_through(data = "{.data}"))
  network$group$add(add_score_category(data = query(cars = "score")))
  dag <- build_dag(query(group = "category"), network = network)
  test_deps <- dag$dag[["group.pass_through"]]
  expect_identical(test_deps, "group.add_score_category")
})

test_that("on entry also takes dependencies from on exit into account", {
  pass_through2 <- pass_through
  network <- init_network("test", projects = c("cars", "group"))
  network$cars$get(get_sample_data())
  network$cars$add(add_score_category(data = query(cars = "score")))
  network$group$on_entry(pass_through(data = "{.data}"))
  network$group$on_exit(pass_through2(data = query(cars = "category")))
  network$group$add(add_score_category(data = query(cars = "score")))
  dag <- build_dag(query(group = "category"), network = network)
  test_deps1 <- dag$dag[["group.pass_through"]]
  test_deps2 <- dag$dag[["group.pass_through2"]]
  expect_identical(test_deps1, c("cars.get_sample_data", "cars.add_score_category"))
  expect_identical(test_deps2, c("cars.add_score_category", "group.pass_through", "group.add_score_category"))
})

test_that("on entry also takes dependencies from on exit into account", {
  pass_through2 <- pass_through
  network <- init_network("test", projects = c("cars", "group", "group2"))
  network$cars$get(get_sample_data())
  network$cars$add(add_score_category(data = query(cars = "score")))
  network$group$on_entry(pass_through(data = "{.data}"))
  network$group$on_exit(pass_through2(data = query(cars = "category")))
  network$group2$on_entry(pass_through(data = "{.data}"))
  network$group2$on_exit(pass_through2(data = "{.data}"))
  network$group$add(add_score_category(data = query(cars = "score")))
  network$group2$add(add_score_category(data = query(cars = "score", group = "category")), vars = "category")
  dag <- build_dag(query(group2 = "category"), network = network)
  test_deps1 <- dag$dag[["group.pass_through"]]
  test_deps2 <- dag$dag[["group.pass_through2"]]
  test_deps3 <- dag$dag[["group2.pass_through"]]
  test_deps4 <- dag$dag[["group2.pass_through2"]]
  expect_identical(test_deps1, c("cars.get_sample_data", "cars.add_score_category"))
  expect_identical(test_deps2, c("cars.add_score_category", "group.pass_through", "group.add_score_category"))
  expect_identical(test_deps3, c("cars.get_sample_data", "group.add_score_category"))
  expect_identical(test_deps4, c("group2.add_score_category"))
})

test_that("on entry and on exit get correct dependencies on depending on different interweaved contexts", {
  pass_through2 <- pass_through
  pass_through3 <- pass_through
  network <- init_network("test", projects = c("cars", "group", "group2"))
  network$cars$get(get_sample_data())
  network$cars$add(add_score_category(data = query(cars = "score")))
  network$group$on_entry(pass_through(data = "{.data}"))
  network$group$on_exit(pass_through2(data = query(cars = "category")))
  network$group2$on_entry(pass_through(data = "{.data}"))
  network$group2$on_exit(pass_through2(data = "{.data}"))
  network$group$add(add_score_category(data = query(cars = "score")))
  network$group2$add(add_score_category(data = query(cars = "score", group = "category")), vars = "category")
  network$group$add(pass_through3(data = query(group2 = "category")), vars = "value")
  dag <- build_dag(query(group = "value"), network = network)
  test_deps1 <- dag$dag[["group.pass_through"]]
  test_deps2 <- dag$dag[["group.pass_through2"]]
  test_deps3 <- dag$dag[["group2.pass_through"]]
  test_deps4 <- dag$dag[["group2.pass_through2"]]
  expect_identical(test_deps1, c("cars.get_sample_data", "cars.add_score_category"))
  expect_identical(test_deps2, c("cars.add_score_category", "group.pass_through", "group.pass_through3" ,"group.add_score_category"))
  expect_identical(test_deps3, c("cars.get_sample_data", "group.add_score_category"))
  expect_identical(test_deps4, c("group2.add_score_category"))
  expect_identical(dag$execution_order, c("cars.get_sample_data", "cars.add_score_category", "group.pass_through",
                                          "group.add_score_category", "group.pass_through2", "group2.pass_through",
                                          "group2.add_score_category", "group2.pass_through2", "group.pass_through",
                                          "group.pass_through3", "group.pass_through2"))
})

test_that("on entry also takes dependencies from on exit into account", {
  skip("Needs to be resolved at clean_wrapper function, polluted context")
  pass_through2 <- pass_through
  pass_through3 <- pass_through
  network <- init_network("test", projects = c("cars", "group", "group2"))
  network$cars$get(get_sample_data())
  network$cars$add(add_score_category(data = query(cars = "score")))
  network$group$on_entry(pass_through(data = "{.data}"))
  network$group2$on_entry(pass_through(data = "{.data}"))
  network$group2$on_exit(pass_through2(data = "{.data}"))
  network$group$add(add_score_category(data = query(cars = "score")))
  network$group2$add(add_score_category(data = query(cars = "score", group = "category")), vars = "category")
  network$group$add(pass_through3(data = query(group2 = "category")), vars = "value")
  network$group$on_exit(pass_through2(data = query(group = "value")))
  dag <- build_dag(query(group = "value"), network = network)
  test_deps1 <- dag$dag[["group.pass_through"]]
  test_deps2 <- dag$dag[["group.pass_through2"]]
  test_deps3 <- dag$dag[["group2.pass_through"]]
  test_deps4 <- dag$dag[["group2.pass_through2"]]
  expect_identical(test_deps1, c("cars.get_sample_data", "cars.add_score_category"))
  expect_identical(test_deps2, c("cars.add_score_category", "group.pass_through", "group.pass_through3" ,"group.add_score_category"))
  expect_identical(test_deps3, c("cars.get_sample_data", "group.add_score_category"))
  expect_identical(test_deps4, c("group2.add_score_category"))
  expect_identical(dag$execution_order, c("cars.get_sample_data", "cars.add_score_category", "group.pass_through",
                                          "group.add_score_category", "group.pass_through2", "group2.pass_through",
                                          "group2.add_score_category", "group2.pass_through2", "group.pass_through",
                                          "group.pass_through3", "group.pass_through2"))
})
