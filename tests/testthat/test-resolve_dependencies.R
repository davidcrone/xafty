test_that("resolve dependencies works with a root dependency", {
  sm <- govern(test_network)
  xafty_list <- query(customer_data = "id")
  resolve_dependencies(projects = "customer_data", xafty_list = xafty_list, network = test_network, sm = sm)
  pulls <- sm$get_pulls("customer_data")
  projects <- sm$get_projects()
  expect_in(pulls, c("id", "name", "score"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, "customer_data.get_sample_data")
})

test_that("resolve dependencies works with an add link", {
  sm <- govern(test_network)
  xafty_list <- query(customer_data = "category")
  resolve_dependencies(projects = "customer_data", xafty_list = xafty_list, network = test_network, sm = sm)
  pulls <- sm$get_pulls("customer_data")
  projects <- sm$get_projects()
  expect_in(pulls, c("category", "score", "name", "id"))
  expect_equal(projects, "customer_data")
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category"))
})

test_that("resolve_dependencies can correctly resolve a join link", {
  network <- test_network
  sm <- govern(network)
  xafty_list <- query(customer_data = "category", occupations = "department")
  resolve_dependencies(projects = c("customer_data", "occupations"), xafty_list = xafty_list, network = network, sm = sm)
  pulls_customer_data <- sm$get_pulls("customer_data")
  pulls_occupations <- sm$get_pulls("occupations")
  projects <- sm$get_projects()
  expect_in(pulls_customer_data, c("category", "score", "name", "id"))
  expect_in(pulls_occupations, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category", "occupations.get_additional_info", "fuse.customer_data.occupations"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects", {
  network <- test_network
  sm <- govern(network)
  xafty_list <- query(customer_data = c("name", "nickname"), occupations = "department")
  resolve_dependencies(projects = c("customer_data", "occupations"), xafty_list = xafty_list, network = network, sm = sm)
  pulls_customer_data <- sm$get_pulls("customer_data")
  pulls_occupations <- sm$get_pulls("occupations")
  projects <- sm$get_projects()
  expect_in(pulls_customer_data, c("category", "score", "name", "id", "nickname"))
  expect_in(pulls_occupations, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category", "occupations.get_additional_info",
                                  "fuse.customer_data.occupations", "customer_data.new_column_from_both_projects"))
})

test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  sm <- govern(network)
  xafty_list <- query(customer_data = "nickname")
  resolve_dependencies(projects = c("customer_data"), xafty_list = xafty_list, network = network, sm = sm)
  pulls_customer_data <- sm$get_pulls("customer_data")
  pulls_occupations <- sm$get_pulls("occupations")
  projects <- sm$get_projects()
  expect_in(pulls_customer_data, c("category", "score", "name", "id", "nickname"))
  expect_in(pulls_occupations, c("department", "id"))
  expect_in(projects, c("customer_data", "occupations"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category", "occupations.get_additional_info",
                                  "fuse.customer_data.occupations", "customer_data.new_column_from_both_projects"))
})


test_that("resolve_dependencies can correctly resolve a column that depends on two projects, but only pulls from one project", {
  network <- test_network
  sm <- govern(network)
  xafty_list <- query(occupations = "department", intelligence = "intelligence", customer_data = c("name", "nickname"))
  resolve_dependencies(projects = c("customer_data", "occupations", "intelligence"), xafty_list = xafty_list, network = network, sm = sm)
  pulls_customer_data <- sm$get_pulls("customer_data")
  pulls_occupations <- sm$get_pulls("occupations")
  pulls_intelligence <- sm$get_pulls("intelligence")
  pulls_map <- sm$get_pulls("map")
  projects <- sm$get_projects()
  expect_in(pulls_customer_data, c("category", "score", "name", "id", "nickname"))
  expect_in(pulls_occupations, c("department", "id"))
  expect_in(pulls_intelligence, c("intelligence", "secret_id"))
  expect_in(pulls_map, c("id", "secret_id"))
  expect_in(projects, c("customer_data", "occupations", "map", "intelligence"))
  execution_order <- resolve_function_stack(sm = sm)
  expect_equal(execution_order, c("customer_data.get_sample_data", "customer_data.add_score_category", "occupations.get_additional_info",
                                  "intelligence.intelligence_date", "map.mapping_data", "map.add_decoded_id", "fuse.customer_data.occupations",
                                  "customer_data.new_column_from_both_projects", "fuse.intelligence.map",  "fuse.customer_data.map"))
})
