test_that("resolve dependencies works with a root dependency", {
  query <- query(customer_data = "id")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query)
  sm <- resolve_dependencies(query_list = globals, network = test_network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("id", "name", "score"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, "customer_data.get_sample_data")
})

test_that("resolve dependencies works with an add link", {
  query <- query(customer_data = "category")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query)
  sm <- resolve_dependencies(query_list = globals, network = test_network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category"))
})

test_that("resolve_dependencies can correctly resolve a join link", {
  network <- test_network
  dag_sm <- build_tree(network)
  query <- query(customer_data = "category", occupations = "department")
  globals <- dots_to_query(network, query)
  dag_sm <- initialize_join_projects(query_list = globals$internal, network = network, dag_sm = dag_sm)
  sm <- resolve_dependencies(query_list = globals, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("occupations.get_additional_info", "customer_data.get_sample_data", "customer_data.add_score_category", "customer_data.join_datasets"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects", {
  network <- test_network
  dag_sm <- build_tree(network)
  query <- query(customer_data = c("name", "nickname"), occupations = "department")
  globals <- dots_to_query(network, query)
  sm <- resolve_dependencies(query_list = globals, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "occupations.get_additional_info", "customer_data.add_score_category",
                                  "customer_data.join_datasets", "customer_data.new_column_from_both_projects"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  query <- query(customer_data = "nickname")
  dag_sm <- build_tree(network)
  globals <- dots_to_query(network, query)
  sm <- resolve_dependencies(query_list = globals, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "occupations.get_additional_info", "customer_data.add_score_category",
                                  "customer_data.join_datasets", "customer_data.new_column_from_both_projects"))
})


test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  query <- query(occupations = "department", intelligence = "intelligence", customer_data = c("name", "nickname")) |>
    add_join_path(path1= c("customer_data", "occupations"), path2 = c("intelligence", "map", "customer_data"))
  dag_sm <- build_tree(network)
  globals <- dots_to_query(network, query)
  dag_sm <- initialize_join_path(join_path = query$join_path, network = network, dag_sm = dag_sm, state_query = query$states)
  dag_sm <- initialize_join_projects(query_list = globals$internal, network = network, dag_sm = dag_sm)
  sm <- resolve_dependencies(query_list = globals, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(query[["intelligence"]]$select, c("intelligence", "secret_id"))
  expect_in(query[["map"]]$select, c("id", "secret_id"))
  expect_in(projects, c("customer_data", "occupations", "map", "intelligence"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_in(execution_order, c("occupations.get_additional_info", "intelligence.intelligence_date", "customer_data.get_sample_data", "customer_data.add_score_category",
                                  "fuse.customer_data.occupations", "customer_data.new_column_from_both_projects", "map.mapping_data", "map.add_decoded_id",
                                  "fuse.customer_data.map", "fuse.intelligence.map"))
  expect_length(execution_order, 10)
})
