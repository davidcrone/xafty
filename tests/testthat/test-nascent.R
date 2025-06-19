test_that("get columns can be retrieved from the network", {
  table_test <- test_network |> nascent(pull_link("customer_data" = c("id", "name", "score")))
  table_expected <- structure(list(id = 1:5, name = c("Alice", "Bob", "Charlie",
      "Diana", "Eve"), score = c(85, 92, 78, 90, 88)), class = "data.frame", row.names = c(NA, -5L))
  expect_identical(table_test, table_expected)
})

test_that("add columns can be retrieved from the network", {
  table_test <- test_network |> nascent(pull_link("customer_data" = c("name", "category")))
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie",
                  "Diana", "Eve"), category = c("Low",
                  "High", "Low", "High", "Low")), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("simple join is performed correctly", {
  table_test <- test_network |> nascent(pull_link("customer_data" = "name", "occupations" = "department"))
  table_expected <- structure(list(name = c("Alice", "Bob", "Charlie", "Diana", "Eve"
  ), department = c("HR", "IT", "Finance", "Marketing", "Sales"
  )), row.names = c(NA, -5L), class = "data.frame")
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a tornado", {
  table_test <- main_network |> nascent(pull_link(side1 = "col1", side2 = "col2", side3 = "col3"))
  table_expected <- data.frame(
    col1 = c("Why", "What", "How"),
    col2 = c("Hallo", "Ja", "Nein"),
    col3 = c("Bonjour", "Salut", "Ca va")
  )
  expect_identical(table_test, table_expected)
})

test_that("nascent can resolve a large network", {

  large_network <- merge_networks(test_network, main_network)

  large_network$map$join(with = "main", join_datasets(main_data = pull_link(map = "id"), extra_data = pull_link(main = "id")))
  test_table <- large_network |> nascent(pull_link(map = "id", intelligence = "new_column",
                    side1 = "col1", side2 = "col2", side3 = "col3"))
  expected_table <- data.frame(
    id = c(1L, 2L, 3L, 4L, 5L),
    new_column = c("HR1", "IT2", "Finance3", "Marketing4", "Sales5"),
    col1 = c("Why", "What", "How", NA_character_, NA_character_),
    col2 = c("Hallo", "Ja", "Nein", NA_character_, NA_character_),
    col3= c("Bonjour", "Salut", "Ca va", NA_character_, NA_character_)
  )
  expect_identical(test_table, expected_table)
})
