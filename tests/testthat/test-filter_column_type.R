test_that("Filter for type 'factor' works", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.character(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no")),
                            "Arrival_Time" = as.character(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!factor", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!factor",
                               "Arrival_Time" = "##!!datetime")

  xafty_column_true <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Name")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")

  expect_equal(xafty_column_true, rep(TRUE, 3))
  expect_equal(xafty_column_false, rep(FALSE, 3))

})

test_that("Filter for type 'character' works", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.numeric(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no")),
                            "Arrival_Time" = as.character(c("2021-09-06 12:34:56", "2021-09-06 11:34:56", "2021-09-06 12:10:01"))
  )

  validity_table <- data.frame("Name" = "##!!factor", "Age" = "##!!text",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!text",
                               "Arrival_Time" = "##!!datetime")

  xafty_column_true <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Age")

  expect_equal(xafty_column_true, rep(TRUE, 3))
  expect_equal(xafty_column_false, rep(FALSE, 3))

})
