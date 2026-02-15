test_that("get columns can be retrieved with characters from the network", {
  table_test <- query("customer_data" = c("id", "name", "score")) |> nascent(test_network)
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie",
      "Diana", "Eve"), score = c(85, 92, 78, 90, 88)), class = "data.frame", row.names = c(NA, -5L))
  expect_identical(table_test, table_expected)
})

test_that("get columns by can be retrieved with symbols from the network", {
  table_test <- query(id, name, score) |> nascent(test_network)
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                   score = c(85, 92, 78, 90, 88)),
                              class = "data.frame", row.names = c(NA, -5L))
  expect_identical(table_test, table_expected)
})

test_that("add columns can be retrieved from the network", {
  table_test <- query("customer_data" = c("name", "category")) |> nascent(test_network)
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie",
                  "Diana", "Eve"), category = c("Low",
                  "High", "Low", "High", "Low")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("simple join is performed correctly", {
  table_test <- query("customer_data" = "name", "occupations" = "department") |> nascent(test_network)
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"
  ), department = c("HR", "IT", "Finance", "Marketing", "Sales"
  )), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a tornado", {
  table_test <- query(side1 = "col1", side2 = "col2", side3 = "col3") |> nascent(main_network)
  table_expected <- data.frame(
    col1 = c("Why", "What", "How"),
    col2 = c("Hallo", "Ja", "Nein"),
    col3 = c("Bonjour", "Salut", "Ca va")
  )
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a large network", {

  large_network <- merge_networks(name = "merged_network", test_network, main_network)

  large_network$map$link(join_datasets(main_data = query(map = "id"), extra_data = query(main = "id")))
  test_table <- query(map = "id", intelligence = "new_column",
                      side1 = "col1", side2 = "col2", side3 = "col3") |> nascent(large_network)
  expected_table <- data.frame(
    id = c(1L, 2L, 3L, 4L, 5L),
    new_column = c("HR1", "IT2", "Finance3", "Marketing4", "Sales5"),
    col1 = c("Why", "What", "How", NA_character_, NA_character_),
    col2 = c("Hallo", "Ja", "Nein", NA_character_, NA_character_),
    col3= c("Bonjour", "Salut", "Ca va", NA_character_, NA_character_)
  )
  expect_identical(test_table, expected_table)
})

test_that("Querying a project with star retrieves all columns associated with the project", {
  test_state_1 <- init_network("test_state_1")
  test_state_1$add_project("customer_data")
  test_state_1$add_project("occupation")
  test_state_1$customer_data$link(get_sample_data())
  test_state_1$customer_data$link(add_score_category(data = query(customer_data = "score")))
  test_data <- query(customer_data = "*") |> nascent(test_state_1)
  expected_data <- structure(list(score = c(85, 92, 78, 90, 88), id = 1:5,
                            name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                            category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("querying with * retrieves all data from a project", {
  test_data <- query(customer_data = "*") |> nascent(test_network)
  expected_data <- structure(list(mean_nickname = c("SmartHRAlice", "DumbITBob",
    "DumbFinanceCharlie", "SmartMarketingDiana", "DumbSalesEve"),
    score = c(85, 92, 78, 90, 88), id = 1:5, name = c("Alice",
    "Bob", "Charlie", "Diana", "Eve"), category = c("Low", "High",
    "Low", "High", "Low"), nickname = c("HRAlice", "ITBob", "FinanceCharlie",
    "MarketingDiana", "SalesEve")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("projects with same column names can be pulled", {
  int_catgeory <- function(data) {
    data$category <- c("right", "left", "left", "right", "left")
    data
  }
  test_network$intelligence$link(int_catgeory(data = query(intelligence = "intelligence")))
  test_data <- query(customer_data = "category", intelligence = "category") |> nascent(test_network)
  expected_data <- structure(list(category = c("Low", "High", "Low", "High", "Low"
  ), category = c("right", "left", "left", "right", "left")), row.names = c(NA,
    -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("nascent two projects that have not yet been joined will raise an errors", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))))
  test_network$occupations$link(get_additional_info())
  expect_error(nascent(query(occupations = "id", customer_data = c("name")), test_network), regexp = "building a join path is not possible")
})

test_that("An unjoined project and a project work seamlessly together in nascent", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$link(get_sample_data())
  test_network$occupations$link(add_score_category(data = query(customer_data = "score")))
  test_data <- query(customer_data = "name", occupations = "category") |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Add function with a non-query argument works in nascent", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  add_score_category <- function(data, na_as_negative = FALSE) {
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
      data
    }
  }
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score"), na_as_negative = TRUE))
  test_data <- query(customer_data = c("name", "category")) |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                              category = c("Low", "High", "Low", "High", "Low")),
                         row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("State argument correclty passes the argument into the state variable", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  add_score_category <- function(data, na_as_negative = "{na_as_negative}") {
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
      data
    }
  }
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score"),
                                                    na_as_negative = "{na_as_negative}"), vars = "category")
  test_data <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = TRUE) |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Flow of functions can be controlled through states", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  add_score_category <- function(data, na_as_negative = "{na_as_negative}") {
    if(is.null(na_as_negative)) na_as_negative <- TRUE
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
    } else {
      data$category <- ifelse(data$score >= 110, "High", "Low")

    }
    data
  }
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")))
  query1 <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = TRUE)
  query2 <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = FALSE)
  test_data1 <- nascent(query1, test_network)
  test_data2 <- nascent(query2, test_network)
  expected_data1 <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                          category = c("Low", "High", "Low", "High", "Low")),
                          row.names = c(NA, -5L), class = "data.frame")
  expected_data2 <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                          category = c("Low", "Low", "Low", "Low", "Low")),
                          row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data1, expected_data1)
  expect_identical(test_data2, expected_data2)
})

test_that("Interpolating a state into a variable name allows to retrieve different columns", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  get_data <- function() {
    data.frame(
      data.2025 = c("A", "B"),
      data.2026 = c("C", "D")
    )
  }
  test_network$customer_data$link(get_data())
  qry1 <- query(customer_data = "data.{year}") |> with_state(year = 2025)
  qry2 <- query(customer_data = "data.{year}") |> with_state(year = 2026)
  tets_data1 <- nascent(qry1, test_network)
  tets_data2 <- nascent(qry2, test_network)
  expected_data1 <- get_data()[, 1, drop = FALSE]
  expected_data2 <- get_data()[, 2, drop = FALSE]
  expect_equal(tets_data1, expected_data1)
  expect_equal(tets_data2, expected_data2)
})

test_that("interpolated dependend query is correctly interpolated and executed", {
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
  query <- query(test_data = c("data.{current_year}", "data.{year}")) |> with_state(current_year = 2027, year = 2025)
  test_data <- nascent(query, test_network)
  expected_data <- structure(list(data.2027 = c("E", "F"), data.2025 = c("A", "B"
  )), row.names = c(NA, -2L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("nascent has access to default state and returns the default value during evalution", {
  test_network <- init_network(name = "test_network", projects = "test_data")
  test_network$add_state("year", default = 2025)
  get_data <- function(var = "{year}") {
    if(var == 2025) {
      data.frame(
        data.2025 = c("A", "B"),
        data.2026 = c("C", "D")
      )
    } else {
      FALSE
    }
  }
  test_network$test_data$link(get_data())
  qry <- query(test_data = "data.{year}")
  test_data <- nascent(qry, test_network)
  expected_data <- data.frame(data.2025 = c("A", "B"))
  expect_identical(test_data, expected_data)
})

test_that("Objects and States are correctly integrated during join_dependencies", {
  qry <- query(occupations = "id", map = "secret_id") |> with_state(column_name = "id")
  test_data <- nascent(qry, test_network)
  expected_data <-structure(list(id = c(1L, 2L, 3L, 4L, 5L),
                                 secret_id = c(2, 3, 4, 5, 6)), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Registering the wrong variable name through vars yields an informative error", {
  network <- init_network("test", projects = "test_proj")
  network$test_proj$link(get_sample_data(), vars = c("id", "nam", "score"))
  qry <- query(test_proj = "nam")
  expect_error(nascent(qry, network), regexp = "variable 'nam' does not appear in the return value of 'get_sample_data'")
})

test_that("On entry is correctly interweaved into the dag and evaluates properly", {
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$add_group("occupations")
  test_network$customer_data$on_entry(increase_score(data = "{.data}"), "increase_score", group = "occupations")
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")), group = "occupations")

  test_data <- query(customer_data = c("name", "category")) |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("High", "High", "High", "High", "High")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Two entry functions are correctly interpolated int the dag and properly evaluated", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  # Here we added name as a dependency, even though add_score_category does not depends on name, but the function
  # decrease_score_of_hated_pupil does
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "group")
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score_of_hated_pupil <- function(data = "{.data}") {
    data$score[data$name == "Alice"] <- 10
    data
  }
  test_network$customer_data$on_entry(name = "increase_score", fun =increase_score(), group = "group")
  test_network$customer_data$on_entry(name = "manipulate_score", fun = decrease_score_of_hated_pupil(), group = "group")
  test_data <- query(customer_data = "name", customer_data = "category") |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "High", "High", "High")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("On entry is correctly interpolated into the dag and evaluates properly", {
  test_network <- init_network(name = "test_network", projects = c("customer_data"))
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")), group = "group")
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score <- function(data = "{.data}") {
    data$score <- data$score - 100
    data
  }
  test_network$customer_data$on_entry(name = "increase_score", fun = increase_score(), group = "group")
  test_network$customer_data$on_exit(name = "decrease_score", fun = decrease_score(query(customer_data = "score")), group = "group")
  test_data <- query(customer_data = c("name", "score", "category")) |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  score = c(85, 92, 78, 90, 88),
  category = c("High", "High", "High", "High", "High")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Both on entry and on exit get data passed to them using {.data}", {
  test_network <- init_network(name = "test_network", projects = c("customer_data"))
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")), group = "group")
  increase_score <- function(data = "{.data}") {
    data
  }
  decrease_score <- function(data = "{.data}") {
    data
  }
  test_network$customer_data$on_entry(name = "increase_score", fun = increase_score(), group = "group")
  test_network$customer_data$on_exit(name = "decrease_score", fun = decrease_score(), group = "group")
  expect_no_error(nascent(query(customer_data = c("name", "score", "category")), test_network))
})

test_that("On entry is correctly interpolated into the dag and evaluates properly", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = "score")), group = "group")
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score <- function(data = "{.data}") {
    data$score <- data$score - 100
    data
  }
  test_network$customer_data$on_entry(name = "increase_score", fun = increase_score(), group = "group")
  test_network$customer_data$on_exit(name = "decrease_score", fun = decrease_score(query(customer_data = "score")), group = "group")
  add_has_passed <- function(data = query(customer_data = "category")) {
    data$has_passed <- ifelse(data$category == "High", TRUE, FALSE)
    data
  }
  test_network$customer_data$link(add_has_passed())
  test_data <- query(customer_data = c("name", "score", "category", "has_passed")) |> nascent(test_network)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  score = c(85, 92, 78, 90, 88),
                                  category = c("High", "High", "High", "High", "High"),
                                  has_passed =c(TRUE, TRUE, TRUE, TRUE, TRUE)),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Dependencies in on_entry are correctly resolved", {
  on_entry_network <- init_network("on_entry", projects = c("cars"))
  on_entry_network$cars$link(test_get_car_data(conn = TRUE))
  on_entry_network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_entry_network$cars$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  on_entry_network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  test_data <- nascent(query(cars = "Tries"), on_entry_network)
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(NA, -3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Dependencies in on_exit are correctly resolved", {
  on_exit_network <- init_network("on_exit", projects = c("cars"))
  on_exit_network$cars$link(test_get_car_data(conn = TRUE))
  on_exit_network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_exit_network$cars$on_exit(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  on_exit_network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  test_data <- nascent(query(cars = "Tries"), on_exit_network)
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(2L, 3L, 1L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("on_exit function does not take duplicated dependencies", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  network <- init_network("on_exit", projects = "cars")
  network$cars$link(test_get_car_data(conn = TRUE))
  network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$cars$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  network$cars$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")), group = "group")
  network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  test_data <- nascent(query(cars = "Tries"), network)
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(1L, 2L, 3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("fucntion in context depending on another function of the same context does not create a cycle", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  add_tries_data_license2 <- function(data) {
    data$Tries2 <- data$Tries + 1
    data
  }
  network <- init_network("on_exit", projects = "cars")
  network$cars$link(test_get_car_data(conn = TRUE))
  network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$cars$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  network$cars$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")), group = "group")
  network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  network$cars$link(add_tries_data_license2(data = query(cars = "Tries")), vars = c("Tries2"), group = "group")
  test_data <- query(cars = c("Tries2")) |> nascent(network)
  expect_data <- structure(list(Tries2 = c(2, 3, 6)), row.names = c(1L, 2L, 3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("foreign function dependend on function with context and needed by a function also in context is correctly ignored by on_entry as dependency", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  add_tries_data_license2 <- function(data) {
    data$Tries2 <- data$Tries + 1
    data
  }
  network <- init_network("on_exit", projects = "cars")
  network$cars$link(test_get_car_data(conn = TRUE))
  network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$cars$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  network$cars$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")), group = "group")
  network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  network$cars$link(add_id_to_car(data = query(cars = "Car_Color", cars = "Tries")))
  network$cars$link(add_tries_data_license2(data = query(cars = c("ID"), cars = "Tries")), group = "group")
  test_data <- nascent(query(cars = c("Tries2")), network)
  expect_data <- structure(list(Tries2 = c(2, 3, 6)), row.names = c(1L, 3L, 2L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Star (*) select only retrieves queries and not objects or contexts", {
  on_entry_network <- init_network("on_entry", projects = "cars")
  on_entry_network$cars$link(test_get_car_data(conn = TRUE))
  on_entry_network$cars$link(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_entry_network$cars$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")), group = "group")
  expect_error(nascent(query(group = "*"), on_entry_network), regexp = "Star select")
  on_entry_network$cars$link(add_tries_data_license(data = query(cars = "Name")), group = "group")
  test_data <- nascent(query(cars = "Tries"), on_entry_network)
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(NA, -3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Simple where filter can be nascented", {
  data <- query(customer_data = "name") |> where(id == 1) |> nascent(test_network)
  expect_identical(data, data.frame(name = "Alice"))
})

test_that("on_entry/on_exit depend on variable with context from a new project also correctly resolves", {
  pass_through2 <- pass_through
  add_score_category2 <- function(data) {
    data$category2 <- ifelse(data$score >= 90, "High", "Low")
    data
  }
  network <- init_network("test", projects = "base")
  network$base$link(get_sample_data())
  network$base$on_entry(pass_through(data = "{.data}"), group = "dependend")
  network$base$link(add_score_category(data = query(base = "score")), group = "dependend")
  network$base$on_entry(pass_through2(data = query(base = "category")), group = "queried")
  network$base$link(add_score_category2(data = query(base = "score")), group = "queried")
  data_test <- query(base = "category2") |> nascent(network)
  data_exp <- structure(list(category2 = c("Low", "High", "Low", "High", "Low"
  )), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(data_test, data_exp)
})

test_that("Simple where filter can be nascented and filters NA values", {
  add_missing_data <- function(data) {
    data$missing <- c(TRUE, FALSE, NA, TRUE, FALSE)
    data
  }
  test_network$customer_data$link(add_missing_data(query(customer_data = "name")))
  data <- query(customer_data = "name") |> where(missing) |> nascent(test_network)
  expect_identical(data$name, c("Alice", "Diana"))
})

test_that("join_dependencies can be resolved as delat adding joins to the join path instead of rewriting it", {
  join_func <- function(join, other) {
    merge(join, other, by = "id", all.x = TRUE)
  }
  join_func2 <- join_func
  network <- init_network("test", projects = c("join1", "join2", "join3"))
  network$join1$link(get_sample_data())
  network$join2$link(get_sample_data())
  network$join3$link(get_sample_data())
  network$join1$link(join_func(join = query(join1 = "id"), other = query(join2 = "id")), direction = "both")
  network$join2$link(add_score_category(query(join1 = "name", join2 = "score")))
  network$join3$link(join_func2(join = query(join3 = "id"), other = query(join2 = c("id", "category"))),  direction = "both")
  test_data <- query(join3 = "id", join2 = "name") |> nascent(network)
  exp_data <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "Diana", "Eve"))
  expect_identical(test_data, exp_data)
})
