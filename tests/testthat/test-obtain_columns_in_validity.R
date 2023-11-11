test_that("Function retrieves columns with xafty syntax correctly", {
  xafty_syntax <- c("##!!text", "##!!eachpattern", "##number")

  validity_table <- data.frame(
    "Plant_Name" = c("##!!text"),
    "Color" = c("##!!text", "##!!eachpattern"),
    "Petal.Size" = c("##number", "#!eachexact")
  )

  columns_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(columns_xafty_pair, c(
    "##!!text" = "Plant_Name", "##!!text" = "Color", "##!!eachpattern" = "Color",
    "##number" = "Petal.Size"
  ))
})

test_that("Returns correct value with minimal validity", {
  xafty_syntax <- "##!!datetime"

  validity_table <- data.frame(
    "Birthday_m" = "##!!datetime",
    "Birthday" = "##!!datetime"
  )

  column_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(column_xafty_pair, c("##!!datetime" = "Birthday_m", "##!!datetime" = "Birthday"))
})

test_that("Returns correct value with possible validity", {
  xafty_syntax <- c("##!!datetime", "##!!patterninput")

  validity_table <- data.frame(
    "Birthday_m" = "##!!datetime",
    "Birthday" = "##!!patterninput", "yes"
  )

  column_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(column_xafty_pair, c("##!!datetime" = "Birthday_m", "##!!patterninput" = "Birthday"))
})

test_that("Returns correct value with possible validity", {
  xafty_syntax <- c("##!!datetime", "##!!patterninput")

  validity_table <- data.frame(
    "Birthday_m" = "##!!datetime",
    "Birthday" = c("##!!datetime", "##!!date")
  )

  column_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(column_xafty_pair, c("##!!datetime" = "Birthday_m", "##!!datetime" = "Birthday"))
})

test_that("NA Values do not pose a problem", {
  xafty_syntax <- c("##!!datetime", "##!!patterninput")

  validity_table <- data.frame(
    "Birthday_m" = c(NA, "##!!datetime"),
    "Birthday" = c("##!!datetime", "##!!date", NA, NA),
    "Arrival" = c(rep(NA, 4))
  )

  column_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(column_xafty_pair, c("##!!datetime" = "Birthday_m", "##!!datetime" = "Birthday"))
})

test_that("Single #! syntax is not flagged as xafty snyrax", {
  xafty_syntax <- c("##!!datetime", "##!!patterninput")

  validity_table <- data.frame(
    "Birthday_m" = c(NA, "#!datetime"),
    "Birthday" = c("#!datetime", "#!date", NA, NA),
    "Arrival" = c(rep(NA, 4))
  )

 expect_warning(obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax))

})

test_that("Single #! syntax is correctly retrieved", {
  xafty_syntax <- c("#!datetime", "#!patterninput")

  validity_table <- data.frame(
    "Birthday_m" = c(NA, "#!datetime"),
    "Birthday" = c("#!datetime", "#!patterninput", NA, NA),
    "Arrival" = c(rep(NA, 4))
  )

  column_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(column_xafty_pair, c("#!datetime" = "Birthday_m", "#!datetime" = "Birthday",
                                    "#!patterninput" = "Birthday"))

})


