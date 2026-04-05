test_that("resolve dependencies works with a root dependency", {
  query <- query(customer_data = "id")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(query, test_network)
  sm <- resolve_dependencies(query_list = globals$internal,  network = test_network, dag_sm = dag_sm)
  query <- sm$get("query")
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("id", "name", "score"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, "customer_data.get_sample_data")
})

test_that("resolve dependencies works with an add link", {
  query <- query(customer_data = "category")
  dag_sm <- build_tree(test_network)
  globals <- dots_to_query(query, test_network)
  sm <- resolve_dependencies(query_list = globals$internal, network = test_network, dag_sm = dag_sm)
  query <- sm$get("query")
  projects <- get_projects(query)
  expect_in(query[["customer_data"]]$select, c("category", "score", "name", "id"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(dag_sm = sm, network = test_network)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category"))
})

test_that("resolve_dependencies can correctly resolve a join link", {
  network <- test_network
  dag_sm <- build_tree(network)
  dag_sm$set_main_project("customer_data")
  query <- query(customer_data = "category", occupations = "department")
  globals <- dots_to_query(query, network)
  sm <- resolve_dependencies(query_list = globals$internal, network = network, dag_sm = dag_sm)
  query <- sm$get("query")
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
  dag_sm$set_main_project("customer_data")
  query <- query(customer_data = c("name", "nickname"), occupations = "department")
  globals <- dots_to_query(query, network)
  sm <- resolve_dependencies(query_list = globals$internal, network = network, dag_sm = dag_sm)
  query <- sm$get("query")
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
  dag_sm$set_main_project("customer_data")
  globals <- dots_to_query(query, network)
  sm <- resolve_dependencies(query_list = globals$internal, network = network, dag_sm = dag_sm)
  query <- sm$get("query")
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
    add_join_path(path1= c("customer_data", "occupations"), path2 = c("intelligence", "map"), path3 = c("customer_data", "map"))
  dag_sm <- build_tree(network)
  globals <- dots_to_query(query, network)
  dag_sm$set_main_project(globals$main)
  dag_sm <- initialize_join_path(join_path = query$join_path, network = network, dag_sm = dag_sm)
  sm <- resolve_dependencies(query_list = globals$internal, network = network, dag_sm = dag_sm)
  query <- sm$get("query")
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
  dag <- list(
    projectA.get_data = character(0),
    group.projectB.group_by_relation = c("projectA.get_data"),
    group.projectB.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    group.projectB.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A", "projectB.ungroup")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper reorders execution order when the function in the wrapper depends on execution of the variable within the context", {
  dag <- list(
    projectA.get_data = character(0),
    group.projectB.group_by_relation = c("projectA.get_data"),
    projectC.add_variable_B = c("projectA.get_data"),
    group.projectB.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    group.projectB.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectC.add_variable_B", "projectB.group.add_variable_A", "projectB.group.ungroup")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper reorders a function to the end of the context when the function depends on one more more variables of the wrapped group", {
  dag <- list(
    projectA.get_data = character(0),
    group.projectB.group_by_relation = c("projectA.get_data"),
    group.projectB.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    group.projectB.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectC.add_variable_B", "projectB.group.ungroup")
  test_order <- clean_wrapper(project = "projectB", group = "group",order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper duplicates a context when a function within the context is broken up", {
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.group_by_relation = c("projectA.get_data"),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectB.group.add_variable_C = c("projectC.add_variable_B", "projectB.group.group_by_relation"),
    projectB.group.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A",
                             "projectC.add_variable_B", "projectB.group.add_variable_C", "projectB.group.ungroup")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B",
                      "projectB.group.group_by_relation", "projectB.group.add_variable_C", "projectB.group.ungroup")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper handles different context with interwoven foreign projects correctly", {
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.group_by_relation = c("projectA.get_data"),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectC.add_variable_E = c("projectA.get_data"),
    projectC.add_variable_D = c("projectB.group.add_variable_A"),
    projectB.group.add_variable_C = c("projectC.add_variable_B", "projectB.group.group_by_relation"),
    projectB.group.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation"),
    projectD.compute_last = c("projectA.get_data", "projectC.add_variable_D")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectC.add_variable_B", "projectC.add_variable_E",
                             "projectC.add_variable_D", "projectB.group.add_variable_C", "projectB.group.ungroup", "projectD.compute_last")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup",
                      "projectC.add_variable_B", "projectC.add_variable_E", "projectC.add_variable_D",
                      "projectB.group.group_by_relation", "projectB.group.add_variable_C", "projectB.group.ungroup",
                      "projectD.compute_last")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper handles multiple foreign nodes interleaved in project context", {
  names(context_network_1$projectB$ruleset$contexts$group$on_entry) <- "group"
  dag <- list(
    projectA.base = character(0),
    projectB.group.group = "projectA.base",
    projectB.group.add_1 = c("projectA.base", "projectB.group.group"),
    projectC.temp1 = "projectA.base",
    projectB.group.add_2 = c("projectA.base", "projectB.group.group"),
    projectD.temp2 = c("projectB.group.add_2", "projectB.group.add_1"),
    projectB.group.ungroup = c("projectB.group.add_2", "projectB.group.group")
  )
  order <- c("projectA.base", "projectB.group.group", "projectB.group.add_1", "projectC.temp1", "projectB.group.add_2", "projectD.temp2", "projectB.group.ungroup")
  test <- clean_wrapper("projectB", group = "group", order = order, dag = dag, contexts = NULL, network = context_network_1)
  expected <- c("projectA.base", "projectB.group.group", "projectB.group.add_1", "projectB.group.add_2", "projectB.group.ungroup", "projectC.temp1", "projectD.temp2")
  expect_identical(test, expected)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = order, dag = dag, network = context_network_1), expected)
})

test_that("clean_wrapper only manipulates the specified project", {
  dag <- list(
    projectA.start = character(0),
    projectB.group.group = "projectA.start",
    projectB.group.add_x = "projectB.group.group",
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.group = "projectA.start",
    projectC.group.add_y = "projectC.group.group",
    projectC.group.ungroup = "projectC.group.add_y"
  )
  order <- c("projectA.start", "projectB.group.group", "projectB.group.add_x", "projectB.group.ungroup",
             "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup")
  expect_identical(clean_wrapper("projectB", group = "group", order = order, dag = dag, contexts = NULL, network = context_network_2), order)
  expect_identical(clean_wrapper("projectC", group = "group", order = order, dag = dag, contexts = NULL, network = context_network_2), order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2), order)
})

test_that("clean_wrapper correctly entangles interleaved contexts from different projects", {
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectB.group.group = c("projectA.start", "projectB.group.add_x"),
    projectB.group.add_x = "projectB.group.group",
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.add_y = c("projectC.group.group", "projectB.group.add_x"),
    projectC.group.ungroup = "projectC.group.add_y"
  )
  order <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.add_x", "projectC.group.add_y", "projectB.group.ungroup", "projectC.group.ungroup")
  epx_orderB <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.add_x", "projectB.group.ungroup", "projectC.group.add_y", "projectC.group.ungroup")
  exp_orderC <- c("projectA.start", "projectB.group.group", "projectB.group.add_x", "projectB.group.ungroup", "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup")
  expect_identical(clean_wrapper("projectB", group = "group", order = order, dag = dag, contexts = NULL, network = context_network_2), epx_orderB)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2), exp_orderC)
})

test_that("clean_all_wrappers correctly entangles interleaved contexts from different projects", {
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectB.group.group = c("projectA.start"),
    projectB.group.add_x = "projectB.group.group",
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.add_y = c("projectC.group.group", "projectB.group.add_x"),
    projectB.group.add_z = c("projectC.group.add_y", "projectB.group.group"),
    projectC.group.ungroup = "projectC.group.add_y"
  )
  order <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.add_x",
             "projectC.group.add_y", "projectB.group.add_z", "projectB.group.ungroup", "projectC.group.ungroup")
  exp_order <- c("projectA.start", "projectB.group.group", "projectB.group.add_x", "projectB.group.ungroup",
                 "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup",
                 "projectB.group.group", "projectB.group.add_z", "projectB.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers correctly disentangles interleaved contexts with two on_entry functions", {
  context_network_2$projectB$ruleset$contexts$group$on_entry <- list(group = list(), filter = list())
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectB.group.group = c("projectA.start"),
    projectB.group.filter = c("projectA.start", "projectB.group.group"),
    projectB.group.add_x = "projectB.group.group",
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.add_y = c("projectC.group.group", "projectB.group.add_x"),
    projectB.group.add_z = c("projectC.group.add_y", "projectB.group.group"),
    projectC.group.ungroup = "projectC.group.add_y"
  )
  order <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.filter", "projectB.group.add_x",
             "projectC.group.add_y", "projectB.group.add_z", "projectB.group.ungroup", "projectC.group.ungroup")
  exp_order <- c("projectA.start", "projectB.group.group", "projectB.group.filter", "projectB.group.add_x",
                 "projectB.group.ungroup", "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup",
                 "projectB.group.group", "projectB.group.filter", "projectB.group.add_z", "projectB.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})


test_that("clean_all_wrappers correctly entangles interleaved contexts with three projects", {
  context_network_2$projectB$ruleset$contexts$group$on_entry <- list(group = list(), filter = list())
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectB.group.group = c("projectA.start"),
    projectB.group.filter = c("projectA.start", "projectB.group.group"),
    projectB.group.add_x = "projectB.group.group",
    projectD.group.group = c("projectA.start"),
    projectD.group.add_var = c("projectB.group.add_x"),
    projectD.group.ungroup = c("projectD.group.add_var"),
    projectC.group.add_y = c("projectC.group.group", "projectB.group.add_x"),
    projectB.group.add_z = c("projectC.group.add_y", "projectB.group.group"),
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.ungroup = "projectC.group.add_y"
  )
  order <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.filter", "projectB.group.add_x",
             "projectD.group.group", "projectD.group.add_var", "projectD.group.ungroup", "projectC.group.add_y", "projectB.group.add_z", "projectB.group.ungroup", "projectC.group.ungroup")
  exp_order <- c("projectA.start",
                 "projectB.group.group", "projectB.group.filter", "projectB.group.add_x", "projectB.group.ungroup",
                 "projectD.group.group", "projectD.group.add_var", "projectD.group.ungroup",
                 "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup",
                 "projectB.group.group", "projectB.group.filter", "projectB.group.add_z", "projectB.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers correctly keeps minimal context compuation when it is possible to compute variables within one context", {
  context_network_2$projectB$ruleset$contexts$group$on_entry <-   list(group = list(), filter = list())
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectB.group.group = c("projectA.start"),
    projectB.group.filter = c("projectA.start", "projectB.group.group"),
    projectB.group.add_x = "projectB.group.group",
    projectC.group.add_y = c("projectC.group.group", "projectB.group.add_x"),
    projectB.group.add_z = c("projectB.group.group"),
    projectB.group.ungroup = "projectB.group.add_x",
    projectC.group.ungroup = "projectC.group.add_y"
  )

  order <- c("projectA.start", "projectC.group.group", "projectB.group.group", "projectB.group.filter",
             "projectB.group.add_x", "projectC.group.add_y", "projectB.group.add_z", "projectB.group.ungroup", "projectC.group.ungroup")
  exp_order <- c("projectA.start",
                 "projectB.group.group", "projectB.group.filter", "projectB.group.add_x", "projectB.group.add_z", "projectB.group.ungroup",
                 "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})

test_that("clean_wrapper moves the functions together even if only an on_entry context is given", {
  network <- list(projectB = list(ruleset = list(contexts = list(group = list(on_entry = list(order = list()), on_exit  = list())))))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.order = c("projectA.get_data"),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.order"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectB.group.add_variable_C = c("projectB.group.order")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.order", "projectB.group.add_variable_A", "projectC.add_variable_B", "projectB.group.add_variable_C")
  expected_order <- c("projectA.get_data", "projectB.group.order", "projectB.group.add_variable_A", "projectB.group.add_variable_C", "projectC.add_variable_B")
  expect_identical(clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = network), expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper moves the functions together even if only an on_exit context is given", {
  network <- list(projectB = list(ruleset = list(contexts = list(group = list(on_entry = list(), on_exit  = list(order = list()))))))
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.order"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectB.group.add_variable_C = c("projectB.group.order"),
    projectB.group.order = c("projectA.get_data")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.order", "projectB.group.add_variable_A", "projectC.add_variable_B", "projectB.group.add_variable_C")
  expected_order <- c("projectA.get_data", "projectB.group.add_variable_A", "projectB.group.add_variable_C", "projectB.group.order", "projectC.add_variable_B")
  expect_identical(clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = network), expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(network), order = execution_order_input, dag = dag, network = network), expected_order)
})

test_that("clean_wrapper mvoes foreign nodes after the context if they depend on a node within the context", {
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.group_by_relation = c("projectA.get_data"),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectC.add_variable_Y = c("projectC.add_variable_B"),
    projectB.group.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectC.add_variable_B", "projectC.add_variable_Y", "projectB.group.ungroup")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B", "projectC.add_variable_Y")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrapper duplicates a context when a function within the context is broken up", {
  dag <- list(
    projectA.get_data = character(0),
    projectB.group.group_by_relation = c("projectA.get_data"),
    projectB.group.add_variable_A = c("projectA.get_data", "projectB.group.group_by_relation"),
    projectC.add_variable_B = c("projectB.group.add_variable_A"),
    projectC.add_variable_Y = c("projectC.add_variable_B"),
    projectB.group.add_variable_C = c("projectC.add_variable_B", "projectB.group.group_by_relation"),
    projectB.group.ungroup = c("projectB.group.add_variable_A", "projectB.group.group_by_relation")
  )
  execution_order_input <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectC.add_variable_B",
                             "projectC.add_variable_Y", "projectB.group.add_variable_C", "projectB.group.ungroup")
  test_order <- clean_wrapper(project = "projectB", group = "group", order = execution_order_input, dag = dag, contexts = NULL, network = context_network_1)
  expected_order <- c("projectA.get_data", "projectB.group.group_by_relation", "projectB.group.add_variable_A", "projectB.group.ungroup", "projectC.add_variable_B", "projectC.add_variable_Y",
                      "projectB.group.group_by_relation", "projectB.group.add_variable_C", "projectB.group.ungroup")
  expect_identical(test_order, expected_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_1), order = execution_order_input, dag = dag, network = context_network_1), expected_order)
})

test_that("clean_wrappers can resolve a dependency on exit", {
  network <- list(group = list(ruleset = list(contexts = list(group = list(on_entry = list(), on_exit  = list(reorder_cars_by_color = list()))))))
  dag <- list(
    cars.test_get_car_data = character(0),
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = "cars.test_get_car_data",
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license", "cars.test_add_car_color")
  )
  order <- c("cars.test_get_car_data", "group.group.add_tries_data_license", "cars.test_add_car_color", "group.group.reorder_cars_by_color")
  test_order <- clean_wrapper(project = "group", group = "group", order = order, dag = dag, contexts = NULL, network = network)
  exp_order <- c("cars.test_get_car_data", "cars.test_add_car_color", "group.group.add_tries_data_license", "group.group.reorder_cars_by_color")
  expect_identical(test_order, exp_order)
  expect_identical(clean_all_wrappers(wrappers = test_build_wrapper_list(network), order = order, dag = dag, network = network), exp_order)
})

test_that("clean_all_wrappers resolves a nested group, that is 'cross-nested'", {
  context_network_2$projectB$ruleset$contexts$group$on_entry <- list(group = list(), filter = list())
  dag <- list(
    projectA.start = character(0),
    projectC.group.group = "projectA.start",
    projectC.group.add_y = c("projectC.group.group"),
    projectB.group.group = c("projectA.start", "projectC.group.add_y"),
    projectB.group.filter = c("projectA.start", "projectB.group.group"),
    projectB.group.add_x = "projectB.group.group",
    projectB.group.add_z = c("projectB.group.group", "projectC.group.add_y"),
    projectC.group.ungroup = "projectC.group.add_y",
    projectB.group.ungroup = "projectB.group.add_x"
  )
  order <- c("projectA.start", "projectC.group.group", "projectC.group.add_y", "projectB.group.group", "projectB.group.filter",
             "projectB.group.add_x", "projectB.group.add_z", "projectB.group.ungroup", "projectC.group.ungroup")
  exp_order <- c("projectA.start", "projectC.group.group", "projectC.group.add_y", "projectC.group.ungroup",
                 "projectB.group.group", "projectB.group.filter", "projectB.group.add_x", "projectB.group.add_z", "projectB.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})

test_that("clean_all_wrappers resolves a polluted context by cloaking the polluted node'", {
  dag <- list(
    projectA.start = character(0),
    projectB.group.group = c("projectA.start"),
    projectB.group.add_x = "projectB.group.group",
    projectC.add_y = c("projectA.start", "projectB.group.add_x"),
    projectB.group.add_z = c("projectB.group.group"),
    projectB.group.ungroup = c("projectB.group.add_x", "projectC.add_y")
  )
  order <- c("projectA.start", "projectB.group.group", "projectB.group.add_x", "projectC.add_y", "projectB.group.add_z", "projectB.group.ungroup")
  exp_order <- c("projectA.start", "projectB.group.group", "projectB.group.add_x", "projectC.add_y", "projectB.group.add_z", "projectB.group.ungroup")
  order_test <- clean_all_wrappers(wrappers = test_build_wrapper_list(context_network_2), order = order, dag = dag, network = context_network_2)
  expect_identical(order_test, exp_order)
})
