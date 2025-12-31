test_that("get columns can be retrieved with characters from the network", {
  table_test <- test_network |> nascent(query("customer_data" = c("id", "name", "score")))
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie",
      "Diana", "Eve"), score = c(85, 92, 78, 90, 88)), class = "data.frame", row.names = c(NA, -5L))
  expect_identical(table_test, table_expected)
})

test_that("get columns by can be retrieved with symbols from the network", {
  table_test <- test_network |> nascent(id, name, score)
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                   score = c(85, 92, 78, 90, 88)),
                              class = "data.frame", row.names = c(NA, -5L))
  expect_identical(table_test, table_expected)
})

test_that("add columns can be retrieved from the network", {
  table_test <- test_network |> nascent(query("customer_data" = c("name", "category")))
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie",
                  "Diana", "Eve"), category = c("Low",
                  "High", "Low", "High", "Low")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("simple join is performed correctly", {
  table_test <- test_network |> nascent(query("customer_data" = "name", "occupations" = "department"))
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"
  ), department = c("HR", "IT", "Finance", "Marketing", "Sales"
  )), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a tornado", {
  table_test <- main_network |> nascent(query(side1 = "col1", side2 = "col2", side3 = "col3"))
  table_expected <- data.frame(
    col1 = c("Why", "What", "How"),
    col2 = c("Hallo", "Ja", "Nein"),
    col3 = c("Bonjour", "Salut", "Ca va")
  )
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a large network", {

  large_network <- merge_networks(name = "merged_network", test_network, main_network)

  large_network$map$join(join_datasets(main_data = query(map = "id"), extra_data = query(main = "id")))
  test_table <- large_network |> nascent(query(map = "id", intelligence = "new_column",
                    side1 = "col1", side2 = "col2", side3 = "col3"))
  expected_table <- data.frame(
    id = c(1, 2, 3, 4, 5),
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
  test_state_1$customer_data$get(get_sample_data())
  test_state_1$customer_data$add(add_score_category(data = query(customer_data = "score")))
  xafty_query <- query(customer_data = "*")
  test_data <- test_state_1 |> nascent(xafty_query)
  expected_data <- structure(list(score = c(85, 92, 78, 90, 88), id = 1:5,
                            name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                            category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("querying with * retrieves all data from a project", {
  test_data <- test_network |> nascent(query(customer_data = "*"))
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
  test_network$intelligence$add(int_catgeory(data = query(intelligence = "intelligence")))
  test_data <- test_network |> nascent(query(customer_data = "category", intelligence = "category"))
  expected_data <- structure(list(category = c("Low", "High", "Low", "High", "Low"
  ), category = c("right", "left", "left", "right", "left")), row.names = c(NA,
    -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("nascent two projects that have not yet been joined will raise an errors", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$customer_data$add(add_score_category(data = query(customer_data = c("score", "name"))))
  test_network$occupations$get(get_additional_info())
  expect_error(nascent(test_network, query(occupations = "id", customer_data = c("name"))), regexp = "building a join path is not possible")
})

test_that("An unjoined project and a project work seamlessly together in nascent", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$occupations$add(add_score_category(data = query(customer_data = "score")))
  test_data <- nascent(test_network, customer_data = "name", occupations= "category")
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Add function with a non-query argument works in nascent", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$get(get_sample_data())
  add_score_category <- function(data, na_as_negative = FALSE) {
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
      data
    }
  }
  test_network$customer_data$add(add_score_category(data = query(customer_data = "score"), na_as_negative = TRUE))
  test_data <- nascent(test_network, customer_data = c("name", "category"))
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                              category = c("Low", "High", "Low", "High", "Low")),
                         row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("State argument correclty passes the argument into the state variable", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$get(get_sample_data())
  add_score_category <- function(data, na_as_negative = "{na_as_negative}") {
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
      data
    }
  }
  test_network$customer_data$add(add_score_category(data = query(customer_data = "score"),
                                                    na_as_negative = "{na_as_negative}"), vars = "category")
  query <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = TRUE)
  test_data <- nascent(test_network, query)
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "Low", "High", "Low")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("nascent can query an object from the network", {
  test_data <- test_network |> nascent(intelligence = c("[active_customers]"))
  expected_data <- data.frame(intelligence = c(120, 130), row.names = c(1L, 4L))
  expect_equal(test_data, expected_data)
})

test_that("nascent can query an object from the network which has an object as dependency", {
  test_data <- test_network |> nascent(intelligence = c("[mean_intelligence]"))
  expected_data <- 125
  expect_equal(test_data, expected_data)
})

test_that("An interwoven object in a network query does execute the object by itself", {
  test_data <- test_network |> nascent(intelligence = c("intelligence_plus_mean"))
  expected_data <- structure(list(intelligence_plus_mean = c(245, 224, 225, 255, 205)),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_equal(test_data, expected_data)
})

test_that("Flow of functions can be controlled through states", {
  test_network <- init_network(name = "test_network", projects = "customer_data")
  test_network$customer_data$get(get_sample_data())
  add_score_category <- function(data, na_as_negative = "{na_as_negative}") {
    if(is.null(na_as_negative)) na_as_negative <- TRUE
    if (na_as_negative) {
      data$category <- ifelse(data$score >= 90, "High", "Low")
    } else {
      data$category <- ifelse(data$score >= 110, "High", "Low")

    }
    data
  }
  test_network$customer_data$add(add_score_category(data = query(customer_data = "score")))
  query1 <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = TRUE)
  query2 <- query(customer_data = c("name", "category")) |> with_state(na_as_negative = FALSE)
  test_data1 <- nascent(test_network, query1)
  test_data2 <- nascent(test_network, query2)
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
  test_network$customer_data$get(get_data())
  qry1 <- query(customer_data = "data.{year}") |> with_state(year = 2025)
  qry2 <- query(customer_data = "data.{year}") |> with_state(year = 2026)
  tets_data1 <- nascent(test_network, qry1)
  tets_data2 <- nascent(test_network, qry2)
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
  test_network$test_data$get(get_data())
  add_data <- function(data) {
    data$data.2027 <- c("E", "F")
    data
  }
  test_network$test_data$add(add_data(data = query(test_data = "data.{year}")))
  query <- query(test_data = c("data.{current_year}", "data.{year}")) |> with_state(current_year = 2027, year = 2025)
  test_data <- nascent(test_network, query)
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
  test_network$test_data$get(get_data())
  qry <- query(test_data = "data.{year}")
  test_data <- nascent(test_network, qry)
  expected_data <- data.frame(data.2025 = c("A", "B"))
  expect_identical(test_data, expected_data)
})

test_that("Objects and States are correctly integrated during join_dependencies", {
  qry <- query(occupations = "id", map = "secret_id") |> with_state(column_name = "id")
  test_data <- nascent(test_network, qry)
  expected_data <-structure(list(id = c(1, 2, 3, 4, 5),
                                 secret_id = c(2, 3, 4, 5, 6)), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Registering the wrong variable name through vars yields an informative error", {
  network <- init_network("test", projects = "test_proj")
  network$test_proj$get(get_sample_data(), vars = c("id", "nam", "score"))
  qry <- query(test_proj = "nam")
  expect_error(nascent(network, qry), regexp = "variable 'nam' does not appear in the return value of 'get_sample_data'")
})

test_that("On entry is correctly interweaved into the dag and evaluates properly", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$occupations$add(add_score_category(data = query(customer_data = "score")))
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  test_network$occupations$on_entry(increase_score(data = "{.data}"), "increase_score")
  test_data <- nascent(test_network, customer_data = "name", occupations= "category")
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("High", "High", "High", "High", "High")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Two entry functions are correctly interpolated int the dag and properly evaluated", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  # Here we added name as a dependency, even though add_score_category does not depends on name, but the function
  # decrease_score_of_hated_pupil does
  test_network$occupations$add(add_score_category(data = query(customer_data = c("score", "name"))))
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score_of_hated_pupil <- function(data = "{.data}") {
    data$score[data$name == "Alice"] <- 10
    data
  }
  test_network$occupations$on_entry(name = "increase_score", fun =increase_score())
  test_network$occupations$on_entry(name = "manipulate_score", fun = decrease_score_of_hated_pupil())
  test_data <- nascent(test_network, customer_data = "name", occupations= "category")
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  category = c("Low", "High", "High", "High", "High")),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("On entry is correctly interpolated into the dag and evaluates properly", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$occupations$add(add_score_category(data = query(customer_data = "score")))
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score <- function(data = "{.data}") {
    data$score <- data$score - 100
    data
  }
  test_network$occupations$on_entry(name = "increase_score", fun = increase_score())
  test_network$occupations$on_exit(name = "decrease_score", fun = decrease_score(query(customer_data = "score")))
  test_data <- nascent(test_network, customer_data = c("name", "score"), occupations= "category")
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  score = c(85, 92, 78, 90, 88),
  category = c("High", "High", "High", "High", "High")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Both on entry and on exit get data passed to them using {.data}", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$occupations$add(add_score_category(data = query(customer_data = "score")))
  increase_score <- function(data = "{.data}") {
    data
  }
  decrease_score <- function(data = "{.data}") {
    data
  }
  test_network$occupations$on_entry(name = "increase_score", fun = increase_score())
  test_network$occupations$on_exit(name = "decrease_score", fun = decrease_score())
  expect_no_error(nascent(test_network, customer_data = c("name", "score"), occupations= "category"))
})

test_that("On entry is correctly interpolated into the dag and evaluates properly", {
  test_network <- init_network(name = "test_network", projects = c("customer_data", "occupations"))
  test_network$customer_data$get(get_sample_data())
  test_network$occupations$add(add_score_category(data = query(customer_data = "score")))
  increase_score <- function(data = "{.data}") {
    data$score <- data$score + 100
    data
  }
  decrease_score <- function(data = "{.data}") {
    data$score <- data$score - 100
    data
  }
  test_network$occupations$on_entry(name = "increase_score", fun = increase_score())
  test_network$occupations$on_exit(name = "decrease_score", fun = decrease_score(query(customer_data = "score")))
  test_network$add_project("final_pass")
  add_has_passed <- function(data = query(occupations = "category")) {
    data$has_passed <- ifelse(data$category == "High", TRUE, FALSE)
    data
  }
  test_network$final_pass$add(add_has_passed())
  test_data <- nascent(test_network, customer_data = c("name", "score"), occupations= "category", final_pass = "has_passed")
  expected_data <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
                                  score = c(85, 92, 78, 90, 88),
                                  category = c("High", "High", "High", "High", "High"),
                                  has_passed =c(TRUE, TRUE, TRUE, TRUE, TRUE)),
                             row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Dependencies in on_entry are correctly resolved", {
  on_entry_network <- init_network("on_entry", projects = c("cars", "group"))
  on_entry_network$cars$get(test_get_car_data(conn = TRUE))
  on_entry_network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_entry_network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  on_entry_network$group$add(add_tries_data_license(data = query(cars = "Name")))
  test_data <- nascent(on_entry_network, query(group = "Tries"))
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(NA, -3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Dependencies in on_exit are correctly resolved", {
  on_exit_network <- init_network("on_exit", projects = c("cars", "group"))
  on_exit_network$cars$get(test_get_car_data(conn = TRUE))
  on_exit_network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_exit_network$group$on_exit(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  on_exit_network$group$add(add_tries_data_license(data = query(cars = "Name")))
  test_data <- nascent(on_exit_network, query(group = "Tries"))
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(2L, 3L, 1L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Non-query default args are available in nascent", {
  state_object_test <- function(data, state = "{state_test}", logical = TRUE) {
    if(logical) {
      data$state <- state
    }
    data
  }
  test_network$add_state(name = "state_test", default = 2)
  test_network$occupations$add_object("object_with_state", state_object_test(data = query(intelligence = "[active_customers]")))
  test_data <- nascent(test_network, query(occupations = "[object_with_state]"))
  expected_data <- structure(list(intelligence = c(120, 130), state = c(2, 2)), row.names = c(1L, 4L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("on_exit function does not take duplicated dependencies", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  network <- init_network("on_exit", projects = c("cars", "group"))
  network$cars$get(test_get_car_data(conn = TRUE))
  network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  network$group$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")))
  network$group$add(add_tries_data_license(data = query(cars = "Name")))
  test_data <- nascent(network, query(group = "Tries"))
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(1L, 2L, 3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("fucntion in context depending on another function of the same context does not create a cycle", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  add_tries_data_license2 <- function(data) {
    data$Tries2 <- data$Tries + 1
    data
  }
  network <- init_network("on_exit", projects = c("cars", "group"))
  network$cars$get(test_get_car_data(conn = TRUE))
  network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  network$group$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")))
  network$group$add(add_tries_data_license(data = query(cars = "Name")))
  network$group$add(add_tries_data_license2(data = query(group = "Tries")), vars = c("Tries2"))
  test_data <- nascent(network, query(group = c("Tries2")))
  expect_data <- structure(list(Tries2 = c(2, 3, 6)), row.names = c(1L, 2L, 3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("foreign function dependend on function with context and needed by a function also in context is correctly ignored by on_entry as dependency", {
  reorder_cars_by_color2 <- reorder_cars_by_color
  add_tries_data_license2 <- function(data) {
    data$Tries2 <- data$Tries + 1
    data
  }
  network <- init_network("on_exit", projects = c("cars", "group"))
  network$cars$get(test_get_car_data(conn = TRUE))
  network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  network$group$on_exit(reorder_cars_by_color2(cars = query(cars = "Car_Color")))
  network$group$add(add_tries_data_license(data = query(cars = "Name")))
  network$cars$add(add_id_to_car(data = query(cars = "Car_Color", group = "Tries")))
  network$group$add(add_tries_data_license2(data = query(cars = c("ID"), group = "Tries")))
  test_data <- nascent(network, query(group = c("Tries2")))
  expect_data <- structure(list(Tries2 = c(2, 3, 6)), row.names = c(1L, 3L, 2L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Star (*) select only retrieves queries and not objects or contexts", {
  on_entry_network <- init_network("on_entry", projects = c("cars", "group"))
  on_entry_network$cars$get(test_get_car_data(conn = TRUE))
  on_entry_network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
  on_entry_network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
  expect_error(nascent(on_entry_network, query(group = "*")), regexp = "Star select")
  on_entry_network$group$add(add_tries_data_license(data = query(cars = "Name")))
  test_data <- nascent(on_entry_network, query(group = "*"))
  expect_data <- structure(list(Tries = c(1L, 2L, 5L)), row.names = c(NA, -3L), class = "data.frame")
  expect_identical(test_data, expect_data)
})

test_that("Simple where filter can be nascented", {
  qry <- query(customer_data = "name") |> where(id == 1)
  data <- nascent(test_network, qry)
  expect_identical(data, data.frame(name = "Alice"))
})

