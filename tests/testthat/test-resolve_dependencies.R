test_that("resolve dependencies works with a root dependency", {
  query <- query(customer_data = "id")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = test_network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("id", "name", "score"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, "customer_data.get_sample_data")
})

test_that("resolve dependencies works with an add link", {
  query <- query(customer_data = "category")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(test_network, query)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = test_network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category"))
})

test_that("resolve_dependencies can correctly resolve a join link", {
  network <- test_network
  dag_sm <- build_tree(network)
  query <- query(customer_data = "category", occupations = "department")
  globals <- dots_to_query(network, query)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, c("occupations.get_additional_info", "customer_data.get_sample_data", "customer_data.add_score_category", "customer_data.join_datasets"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects", {
  network <- test_network
  dag_sm <- build_tree(network)
  query <- query(customer_data = c("name", "nickname"), occupations = "department")
  globals <- dots_to_query(network, query)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, c("customer_data.get_sample_data", "occupations.get_additional_info", "customer_data.add_score_category",
                                  "customer_data.join_datasets", "customer_data.new_column_from_both_projects"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  query <- query(customer_data = "nickname")
  dag_sm <- build_tree(network)
  globals <- dots_to_query(network, query)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, c("customer_data.get_sample_data", "occupations.get_additional_info", "customer_data.add_score_category",
                                  "customer_data.join_datasets", "customer_data.new_column_from_both_projects"))
})


test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  query <- query(occupations = "department", intelligence = "intelligence", customer_data = c("name", "nickname")) |>
    add_join_path(path1= c("customer_data", "occupations"), path2 = c("intelligence", "map", "customer_data"))
  dag_sm <- build_tree(network)
  globals <- dots_to_query(network, query)
  dag_sm <- initialize_join_path(join_path = query$join_path, dag_sm = dag_sm)
  sm <- resolve_dependencies(query_list = globals$internal, state_list = globals$states, network = network, dag_sm = dag_sm)
  query <- sm$get_query()
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id", "nickname"))
  expect_in(query[["occupations"]]$select, c("department", "id"))
  expect_in(query[["intelligence"]]$select, c("intelligence", "secret_id"))
  expect_in(query[["map"]]$select, c("id", "secret_id"))
  expect_in(projects, c("customer_data", "occupations", "map", "intelligence"))
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_in(execution_order, c("occupations.get_additional_info", "intelligence.intelligence_date", "customer_data.get_sample_data", "customer_data.add_score_category",
                               "customer_data.join_datasets", "customer_data.new_column_from_both_projects", "map.mapping_data", "intelligence.join_datasets_map",
                               "map.add_decoded_id", "customer_data.join_intelligence"))
  expect_length(execution_order, 10)
})

test_that("clean_wrapper keeps the execution order as is when the project functions are correctly wrapped", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation",on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A", "projectB.ungroup")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper reorders execution order when the function in the wrapper depends on execution of the variable within the context", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation",on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectC.add_variable_B = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectC.add_variable_B", "projectB.add_variable_A", "projectB.ungroup")
  test_order <- clean_wrapper(project = "projectB",order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper reorders a function to the end of the context when the function depends on one more more variables of the wrapped group", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation", on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectC.add_variable_B", "projectB.ungroup")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper duplicates a context when a function within the context is broken up", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation", on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectB.add_variable_C = c("projectC.add_variable_B", "projectB.group_by_relation"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectC.add_variable_B", "projectB.add_variable_C", "projectB.ungroup")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B",
                      "projectB.group_by_relation", "projectB.add_variable_C", "projectB.ungroup")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper handles different context with interwoven foreign projects correctly", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation", on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectC.add_variable_E = c("projectA.get_data"),
    projectC.add_variable_D = c("projectB.add_variable_A"),
    projectB.add_variable_C = c("projectC.add_variable_B", "projectB.group_by_relation"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation"),
    projectD.compute_last = c("projectA.get_data", "projectC.add_variable_D")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectC.add_variable_B", "projectC.add_variable_E",
                             "projectC.add_variable_D", "projectB.add_variable_C", "projectB.ungroup", "projectD.compute_last")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B", "projectC.add_variable_E",
                      "projectC.add_variable_D", "projectB.group_by_relation", "projectB.add_variable_C", "projectB.ungroup", "projectD.compute_last")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper handles multiple foreign nodes interleaved in project context", {
  dag <- list(
    projectA.base = character(0),
    projectB.group = "projectA.base",
    projectB.add_1 = c("projectA.base", "projectB.group"),
    projectC.temp1 = "projectA.base",
    projectB.add_2 = c("projectA.base", "projectB.group"),
    projectD.temp2 = c("projectB.add_2", "projectB.add_1"),
    projectB.ungroup = c("projectB.add_2", "projectB.group")
  )
  network <- list(projectB = list(wrappers = list(on_entry = "group", on_exit = "ungroup")))
  order <- c("projectA.base", "projectB.group", "projectB.add_1", "projectC.temp1", "projectB.add_2", "projectD.temp2", "projectB.ungroup")
  test <- clean_wrapper("projectB", order = order, dag = dag, contexts = NULL, network = network)
  expected <- c("projectA.base", "projectB.group", "projectB.add_1", "projectB.add_2", "projectB.ungroup", "projectC.temp1", "projectD.temp2")
  expect_identical(test, expected)
  expect_identical(clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network), expected)
})

test_that("clean_wrapper only manipulates the specified project", {
  dag <- list(
    projectA.start = character(0),
    projectB.group = "projectA.start",
    projectB.add_x = "projectB.group",
    projectB.ungroup = "projectB.add_x",
    projectC.group = "projectA.start",
    projectC.add_y = "projectC.group",
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectB = list(wrappers = list(on_entry = "group", on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectB.group", "projectB.add_x", "projectB.ungroup",
             "projectC.group", "projectC.add_y", "projectC.ungroup")
  expect_identical(clean_wrapper("projectB", order = order, dag = dag, contexts = NULL, network = network), order)
  expect_identical(clean_wrapper("projectC", order = order, dag = dag, contexts = NULL, network = network), order)
  expect_identical(clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network), order)
})

test_that("clean_wrapper correctly entangles interleaved contexts from different projects", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectB.group = c("projectA.start", "projectB.add_x"),
    projectB.add_x = "projectB.group",
    projectB.ungroup = "projectB.add_x",
    projectC.add_y = c("projectC.group", "projectB.add_x"),
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectB = list(wrappers = list(on_entry = "group", on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectB.group", "projectB.add_x", "projectC.add_y", "projectB.ungroup", "projectC.ungroup")
  epx_orderB <- c("projectA.start", "projectC.group", "projectB.group", "projectB.add_x", "projectB.ungroup", "projectC.add_y", "projectC.ungroup")
  exp_orderC <- c("projectA.start", "projectB.group", "projectB.add_x", "projectB.ungroup", "projectC.group", "projectC.add_y", "projectC.ungroup")
  expect_identical(clean_wrapper("projectB", order = order, dag = dag, contexts = NULL, network = network), epx_orderB)
  expect_identical(clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network), exp_orderC)
})

test_that("clean_all_wrappers correctly entangles interleaved contexts from different projects", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectB.group = c("projectA.start"),
    projectB.add_x = "projectB.group",
    projectB.ungroup = "projectB.add_x",
    projectC.add_y = c("projectC.group", "projectB.add_x"),
    projectB.add_z = c("projectC.add_y", "projectB.group"),
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectA = list(wrappers = list(on_entry = NULL, on_exit = NULL)),
    projectB = list(wrappers = list(on_entry = "group", on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectB.group", "projectB.add_x", "projectC.add_y", "projectB.add_z", "projectB.ungroup", "projectC.ungroup")
  exp_order <- c("projectA.start", "projectB.group", "projectB.add_x", "projectB.ungroup", "projectC.group", "projectC.add_y", "projectC.ungroup",
                 "projectB.group", "projectB.add_z", "projectB.ungroup")
  order_test <- clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers correctly entangles interleaved contexts with two on_entry functions", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectB.group = c("projectA.start"),
    projectB.filter = c("projectA.start", "projectB.group"),
    projectB.add_x = "projectB.group",
    projectB.ungroup = "projectB.add_x",
    projectC.add_y = c("projectC.group", "projectB.add_x"),
    projectB.add_z = c("projectC.add_y", "projectB.group"),
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectA = list(wrappers = list(on_entry = NULL, on_exit = NULL)),
    projectB = list(wrappers = list(on_entry = c("group", "filter"), on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectB.group", "projectB.filter", "projectB.add_x", "projectC.add_y", "projectB.add_z", "projectB.ungroup", "projectC.ungroup")
  exp_order <- c("projectA.start", "projectB.group", "projectB.filter", "projectB.add_x", "projectB.ungroup", "projectC.group", "projectC.add_y", "projectC.ungroup",
                 "projectB.group", "projectB.filter", "projectB.add_z", "projectB.ungroup")
  order_test <- clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers correctly entangles interleaved contexts with three projects", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectB.group = c("projectA.start"),
    projectB.filter = c("projectA.start", "projectB.group"),
    projectB.add_x = "projectB.group",
    projectD.group = c("projectA.start"),
    projectD.add_var = c("projectB.add_x"),
    projectD.ungroup = c("projectD.add_var"),
    projectC.add_y = c("projectC.group", "projectB.add_x"),
    projectB.add_z = c("projectC.add_y", "projectB.group"),
    projectB.ungroup = "projectB.add_x",
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectA = list(wrappers = list(on_entry = NULL, on_exit = NULL)),
    projectB = list(wrappers = list(on_entry = c("group", "filter"), on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup")),
    projectD = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectB.group", "projectB.filter", "projectB.add_x", "projectD.group", "projectD.add_var", "projectD.ungroup", "projectC.add_y", "projectB.add_z", "projectB.ungroup", "projectC.ungroup")
  exp_order <- c("projectA.start",
                 "projectB.group", "projectB.filter", "projectB.add_x", "projectB.ungroup",
                 "projectD.group", "projectD.add_var", "projectD.ungroup",
                 "projectC.group", "projectC.add_y", "projectC.ungroup",
                 "projectB.group", "projectB.filter", "projectB.add_z", "projectB.ungroup")
  order_test <- clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers correctly keeps minimal context compuation when it is possible to compute variables within one context", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectB.group = c("projectA.start"),
    projectB.filter = c("projectA.start", "projectB.group"),
    projectB.add_x = "projectB.group",
    projectC.add_y = c("projectC.group", "projectB.add_x"),
    projectB.add_z = c("projectB.group"),
    projectB.ungroup = "projectB.add_x",
    projectC.ungroup = "projectC.add_y"
  )
  network <- list(
    projectA = list(wrappers = list(on_entry = NULL, on_exit = NULL)),
    projectB = list(wrappers = list(on_entry = c("group", "filter"), on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectB.group", "projectB.filter", "projectB.add_x", "projectC.add_y", "projectB.add_z", "projectB.ungroup", "projectC.ungroup")
  exp_order <- c("projectA.start",
                 "projectB.group", "projectB.filter", "projectB.add_x", "projectB.add_z", "projectB.ungroup",
                 "projectC.group", "projectC.add_y", "projectC.ungroup")
  order_test <- clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network)
  expect_identical(order_test, exp_order)
})

test_that("clean_wrapper moves the functions together even if only an on_entry context is given", {
  network <- list(projectB = list(wrappers = list(on_entry = "order", on_exit = NULL)))
  dag <- list(
    projectA.get_data = character(0),
    projectB.order = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.order"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectB.add_variable_C = c("projectB.order")
  )
  execution_order_input <- c("projectA.get_data", "projectB.order", "projectB.add_variable_A", "projectC.add_variable_B", "projectB.add_variable_C")
  expected_order <- c("projectA.get_data", "projectB.order", "projectB.add_variable_A", "projectB.add_variable_C", "projectC.add_variable_B")
  expect_identical(clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network), expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper moves the functions together even if only an on_exit context is given", {
  network <- list(projectB = list(wrappers = list(on_entry = NULL, on_exit = "order")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.add_variable_A = c("projectA.get_data", "projectB.order"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectB.add_variable_C = c("projectB.order"),
    projectB.order = c("projectA.get_data")
  )
  execution_order_input <- c("projectA.get_data", "projectB.order", "projectB.add_variable_A", "projectC.add_variable_B", "projectB.add_variable_C")
  expected_order <- c("projectA.get_data", "projectB.add_variable_A", "projectB.add_variable_C", "projectB.order", "projectC.add_variable_B")
  expect_identical(clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network), expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper duplicates a context when a function within the context is broken up", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation", on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectC.add_variable_Y = c("projectC.add_variable_B"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectC.add_variable_B", "projectC.add_variable_Y", "projectB.ungroup")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B", "projectC.add_variable_Y")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper duplicates a context when a function within the context is broken up", {
  network <- list(projectB = list(wrappers = list(on_entry = "group_by_relation", on_exit  = "ungroup")))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group_by_relation = c("projectA.get_data"),
    projectB.add_variable_A = c("projectA.get_data", "projectB.group_by_relation"),
    projectC.add_variable_B = c("projectB.add_variable_A"),
    projectC.add_variable_Y = c("projectC.add_variable_B"),
    projectB.add_variable_C = c("projectC.add_variable_B", "projectB.group_by_relation"),
    projectB.ungroup = c("projectB.add_variable_A", "projectB.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectC.add_variable_B",
                             "projectC.add_variable_Y", "projectB.add_variable_C", "projectB.ungroup")
  test_order <- clean_wrapper(project = "projectB", order = execution_order_input, dag = dag, contexts = NULL, network = network)
  expected_order <- c("projectA.get_data", "projectB.group_by_relation", "projectB.add_variable_A", "projectB.ungroup", "projectC.add_variable_B", "projectC.add_variable_Y",
                      "projectB.group_by_relation", "projectB.add_variable_C", "projectB.ungroup")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrappers can resolve a dependency on exit", {
  dag <- list(
    cars.test_get_car_data = character(0),
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = "cars.test_get_car_data",
    group.reorder_cars_by_color = c("group.add_tries_data_license", "cars.test_add_car_color")
  )
  network <- list(group = list(wrappers = list(on_entry = NULL, on_exit  = "reorder_cars_by_color")))
  order <- c("cars.test_get_car_data", "group.add_tries_data_license", "cars.test_add_car_color", "group.reorder_cars_by_color")
  test_order <- clean_wrapper(project = "group", order = order, dag = dag, contexts = NULL, network = network)
  exp_order <- c("cars.test_get_car_data", "cars.test_add_car_color", "group.add_tries_data_license", "group.reorder_cars_by_color")
  expect_identical(test_order, exp_order)
  expect_identical(clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network), exp_order)
})

test_that("clean_all_wrappers resolves a nested group, that is 'cross-nested'", {
  dag <- list(
    projectA.start = character(0),
    projectC.group = "projectA.start",
    projectC.add_y = c("projectC.group"),
    projectB.group = c("projectA.start", "projectC.add_y"),
    projectB.filter = c("projectA.start", "projectB.group"),
    projectB.add_x = "projectB.group",
    projectB.add_z = c("projectB.group", "projectC.add_y"),
    projectC.ungroup = "projectC.add_y",
    projectB.ungroup = "projectB.add_x"
  )
  network <- list(
    projectA = list(wrappers = list(on_entry = NULL, on_exit = NULL)),
    projectB = list(wrappers = list(on_entry = c("group", "filter"), on_exit = "ungroup")),
    projectC = list(wrappers = list(on_entry = "group", on_exit = "ungroup"))
  )
  order <- c("projectA.start", "projectC.group", "projectC.add_y", "projectB.group", "projectB.filter", "projectB.add_x", "projectB.add_z", "projectB.ungroup", "projectC.ungroup")
  exp_order <- c("projectA.start", "projectC.group", "projectC.add_y", "projectC.ungroup",
                 "projectB.group", "projectB.filter", "projectB.add_x", "projectB.add_z", "projectB.ungroup")
  order_test <- clean_all_wrappers(projects = names(network), order = order, dag = dag, network = network)
  expect_identical(order_test, exp_order)
})
