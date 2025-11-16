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
