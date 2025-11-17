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

test_that("get_upstream_dependencies returns only relevant dependencies to the extracted pipeline", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = "cars.test_get_car_data",
    group.reorder_cars_by_color = c("group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = "group.reorder_cars_by_color")
  exp_deps <- c("cars.test_add_car_color")
  expect_identical(test_deps, exp_deps)
})

test_that("get_upstream_dependencies returns immediate group dependency", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "group.add_tries_data_license"),
    group.reorder_cars_by_color = c("group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = "group.reorder_cars_by_color")
  exp_deps <- c("cars.test_add_car_color", "group.add_tries_data_license")
  expect_identical(test_deps, exp_deps)
})

test_that("get_upstream_dependencies returns whole chain of thought leading up to group dependency", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.depends_on_group = "group.add_tries_data_license",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.reorder_cars_by_color = c("group.add_tries_data_license", "cars.test_add_car_color")
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = "group.reorder_cars_by_color")
  exp_deps <- c("cars.test_add_car_color", "cars.depends_on_group", "group.add_tries_data_license")
  expect_identical(test_deps, exp_deps)
})

test_that("A target idependent of foreign functions returns an empty character", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.depends_on_group = "group.add_tries_data_license",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.reorder_cars_by_color = c("group.add_tries_data_license")
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = "group.reorder_cars_by_color")
  exp_deps <- c(character(0))
  expect_identical(test_deps, exp_deps)
})

test_that("Targets idependent of foreign functions returns an empty character", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.reorder_cars_by_color = "group.add_tries_data_license",
    group.reorder_cars_by_id = "group.reorder_cars_by_color"
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = c("group.reorder_cars_by_color", "group.reorder_cars_by_id"))
  exp_deps <- c(character(0))
  expect_identical(test_deps, exp_deps)
})

test_that("Several Targets while only one depends on foreign functions returns the dependency", {
  dag <- list(
    group.add_tries_data_license = "cars.test_get_car_data",
    cars.test_add_car_color = c("cars.test_get_car_data", "cars.depends_on_group"),
    group.reorder_cars_by_color = c("group.add_tries_data_license", "cars.test_get_car_data"),
    group.reorder_cars_by_id = c("group.reorder_cars_by_color", "cars.test_add_car_color")
  )
  test_deps <- get_upstream_dependencies(project = "group", dag = dag, targets = c("group.reorder_cars_by_color", "group.reorder_cars_by_id"))
  exp_deps <- c("cars.test_add_car_color")
  expect_identical(test_deps, exp_deps)
})
