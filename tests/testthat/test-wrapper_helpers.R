test_that("splitting a simple order into start and end", {
  extract <-  c("projectA.add_y", "projectB.add_x")
  test_pack <- package_wrapper(project = "projectA", extract = extract)
  exp_pack <- list(
    start = "projectA.add_y",
    end = "projectB.add_x",
    packages = list()
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting simple order with more characters in each group into start and end", {
  extract <-  c("projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy")
  test_pack <- package_wrapper(project = "projectA", extract = extract)
  exp_pack <- list(
    start = c("projectA.add_y", "projectA.add_z"),
    end = c("projectB.add_x", "projectB.add_xy"),
    packages = list()
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting an order into a package", {
  extract <-  c("projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy")
  test_pack <- package_wrapper(project = "projectB", extract = extract)
  exp_pack <- list(
    start = character(0),
    end = character(0),
    packages = list(
      list(
        foreign = c("projectA.add_y", "projectA.add_z"),
        project = c("projectB.add_x", "projectB.add_xy")
      )
    )
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting an order into with packages", {
  extract <-  c("projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy", "projectC.add_1", "projectB.unearth")
  test_pack <- package_wrapper(project = "projectB", extract = extract)
  exp_pack <- list(
    start = character(0),
    end = character(0),
    packages = list(
      list(
        foreign = c("projectA.add_y", "projectA.add_z"),
        project = c("projectB.add_x", "projectB.add_xy")
      ),
      list(
        foreign = c("projectC.add_1"),
        project = c("projectB.unearth")
      )
    )
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting an order into a package", {
  extract <-  c("projectB.add_a", "projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy")
  test_pack <- package_wrapper(project = "projectB", extract = extract)
  exp_pack <- list(
    start = "projectB.add_a",
    end = character(0),
    packages = list(
      list(
        foreign = c("projectA.add_y", "projectA.add_z"),
        project = c("projectB.add_x", "projectB.add_xy")
      )
    )
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting an order into a package", {
  extract <-  c("projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy", "projectA.add_a")
  test_pack <- package_wrapper(project = "projectB", extract = extract)
  exp_pack <- list(
    start = character(0),
    end = "projectA.add_a",
    packages = list(
      list(
        foreign = c("projectA.add_y", "projectA.add_z"),
        project = c("projectB.add_x", "projectB.add_xy")
      )
    )
  )
  expect_identical(test_pack, exp_pack)
})

test_that("splitting an order into a package", {
  extract <-  c("projectB.add_xx", "projectA.add_y", "projectA.add_z", "projectB.add_x", "projectB.add_xy", "projectA.add_a")
  test_pack <- package_wrapper(project = "projectB", extract = extract)
  exp_pack <- list(
    start = "projectB.add_xx",
    end = "projectA.add_a",
    packages = list(
      list(
        foreign = c("projectA.add_y", "projectA.add_z"),
        project = c("projectB.add_x", "projectB.add_xy")
      )
    )
  )
  expect_identical(test_pack, exp_pack)
})

test_that("classify_foreign_dependencies returns only relevant dependencies to the extracted pipeline", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = "cars.test_get_car_data",
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = "group.group.reorder_cars_by_color")
  expect_identical(test_deps$before, "cars.test_add_car_color")
  expect_identical(test_deps$pollution, character(0))
})

test_that("classify_foreign_dependencies returns polluted foreign nodes", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "group.group.add_tries_data_license"),
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = "group.group.reorder_cars_by_color")
  expect_identical(test_deps$before, character(0))
  expect_identical(test_deps$pollution, "cars.test_add_car_color")
})

test_that("classify_foreign_dependencies returns the whole polluted chain of thought", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.depends_on_group = "group.group.add_tries_data_license",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group",  dag = dag, targets = "group.group.reorder_cars_by_color")
  expect_identical(test_deps$before, character(0))
  expect_identical(test_deps$pollution, c("cars.test_add_car_color", "cars.depends_on_group"))
})

test_that("A target idependent of foreign functions returns an empty character", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.depends_on_group = "group.group.add_tries_data_license",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = "group.group.reorder_cars_by_color")
  expect_identical(test_deps$before, character(0))
  expect_identical(test_deps$pollution, character(0))
})

test_that("Targets idependent of foreign functions returns an empty character", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.group.reorder_cars_by_color = "group.group.add_tries_data_license",
    group.group.reorder_cars_by_id = "group.group.reorder_cars_by_color"
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = c("group.group.reorder_cars_by_color", "group.group.reorder_cars_by_id"))
  expect_identical(test_deps$before, character(0))
  expect_identical(test_deps$pollution, character(0))
})

test_that("Targets depend only on domestic, while dependend domestics depend on foreign function", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.group.reorder_cars_by_color = "cars.test_add_car_color",
    group.group.reorder_cars_by_id = "group.group.reorder_cars_by_color"
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = c("group.group.reorder_cars_by_color", "group.group.reorder_cars_by_id"))
  expect_identical(test_deps$before, "cars.test_add_car_color")
  expect_identical(test_deps$pollution, character(0))
})

test_that("Several Targets while only one depends on foreign functions returns the dependency", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.group.reorder_cars_by_color = c("group.group.add_tries_data_license", "cars.test_get_car_data"),
    group.group.reorder_cars_by_id = c("group.group.reorder_cars_by_color", "cars.test_add_car_color")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = c("group.group.reorder_cars_by_color", "group.group.reorder_cars_by_id"))
  expect_identical(test_deps$before, "cars.test_add_car_color")
  expect_identical(test_deps$pollution, character(0))
})

test_that("Target with both polluting and before are correctly classified", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    cars.polluter = "group.group.add_tries_data_license",
    group.group.reorder_cars_by_id = c("group.group.add_tries_data_license", "cars.test_add_car_color", "cars.polluter")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = "group.group.reorder_cars_by_id")
  expect_identical(test_deps$before, "cars.test_add_car_color")
  expect_identical(test_deps$pollution, "cars.polluter")
})

test_that("Target with both polluting from different sources are correctly classified", {
  dag <- list(
    group.group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group", "group.group.add_tries_data_license"),
    cars.polluter = "cars.test_add_car_color",
    group.group.reorder_cars_by_id = c("group.group.add_tries_data_license", "cars.test_add_car_color", "cars.polluter")
  )
  test_deps <- classify_foreign_dependencies(project = "group", group = "group", dag = dag, targets = "group.group.reorder_cars_by_id")
  expect_identical(test_deps$before, character(0))
  expect_in(test_deps$pollution, c("cars.polluter", "cars.test_add_car_color"))
})

test_that("Target depends on context group, correctly classyging the whole context as polluter", {
  skip("Needs to be resolved at clean_wrapper function, polluted context")
  dag <- list(
    cars.get_sample_data = character(0),
    cars.add_score_category = "cars.get_sample_data",
    group.pass_through = "cars.get_sample_data",
    group.add_score_category = c("cars.get_sample_data", "group.pass_through"),
    group2.pass_through = "cars.get_sample_data",
    group2.add_score_category = c("cars.get_sample_data", "group.add_score_category", "group2.pass_through"),
    group2.pass_through2 = "group2.add_score_category",
    group.pass_through2 = c("group2.add_score_category", "group.pass_through", "group.add_score_category")
  )
  contexts <- list(
    "projcetC.context_1" = c("group2.pass_through", "group2.add_score_category", "group2.pass_through2")
  )
  test_deps <- classify_foreign_dependencies(project = "group", dag = dag, targets = c("group.pass_through2"), contexts = contexts)
  exp_deps <- c("group2.add_score_category", "group.add_score_category")
  expect_identical(test_deps, exp_deps)
})

test_that("Target depends on context group, correctly classyging the whole context as polluter", {
  skip("Needs to be resolved at clean_wrapper function, polluted context")
  dag <- list(
    cars.get_sample_data = character(0),
    cars.add_score_category = "cars.get_sample_data",
    group.pass_through = "cars.get_sample_data",
    group.add_score_category = c("cars.get_sample_data", "group.pass_through"),
    group2.pass_through = "cars.get_sample_data",
    group2.add_score_category = c("cars.get_sample_data", "group.add_score_category", "group2.pass_through"),
    group2.pass_through2 = "group2.add_score_category",
    group.pass_through3 = c("group2.add_score_category", "group.pass_through"),
    group.pass_through2 = c("group.pass_through3", "group.pass_through", "group.add_score_category")
  )
  test_deps <- classify_foreign_dependencies(project = "group", dag = dag, targets = c("group.pass_through2"))
  exp_deps <- c("group2.add_score_category", "group.add_score_category")
  expect_identical(test_deps, exp_deps)
})
