test_that("get columns can be retrieved from the network", {
  table_test <- test_network |> nascent(query("customer_data" = c("id", "name", "score")))
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie",
      "Diana", "Eve"), score = c(85, 92, 78, 90, 88)), class = "data.frame", row.names = c(NA, -5L))
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
  query <- query(customer_data = c("name", "category")) |> with(na_as_negative = TRUE)
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
  query1 <- query(customer_data = c("name", "category")) |> with(na_as_negative = TRUE)
  query2 <- query(customer_data = c("name", "category")) |> with(na_as_negative = FALSE)
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
  qry1 <- query(customer_data = "data.{year}") |> with(year = 2025)
  qry2 <- query(customer_data = "data.{year}") |> with(year = 2026)
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
  query <- query(test_data = c("data.{current_year}", "data.{year}")) |> with(current_year = 2027, year = 2025)
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
  qry <- query(occupations = "id", map = "secret_id") |> with(column_name = "id")
  test_data <- nascent(test_network, qry)
  expected_data <-structure(list(id = c(1, 2, 3, 4, 5),
                                 secret_id = c(2, 3, 4, 5, 6)), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(test_data, expected_data)
})

test_that("Registering the wrong column name through vars yields an informative error", {
  network <- init_network("test", projects = "test_proj")
  network$test_proj$get(get_sample_data(), vars = c("id", "nam", "score"))
  qry <- query(test_proj = "nam")
  expect_error(nascent(network, qry), regexp = "Variable 'nam' in project 'test_proj' is not present in the data")
})
