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

test_that("Filter for type 'date' works", {

  check_table <- data.frame("Birthday" = as.character(c("2000-12-24", "1993-09-06",
                                                   "2022-01-01", "01.02.2023",
                                                   "12/01/1993", "2000/12/01")),
                            "Birthday_m" = as.character(c("2000-412-24", "1993/09-06",
                                                        "2022-xx01-01", "01..02.2023",
                                                        "12/01//1993", "20000/12/01")),
                            "Arrival_Date" = as.character(c("2003/12/24", "Christmas", "50404",
                                                            "20.12.12", NA, "Tesla"))
  )

  validity_table <- data.frame("Birthday_m" = "##!!date", "Age" = "##!!text",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!text",
                               "Arrival_Date" = "##!!date")

  xafty_column_true <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Birthday")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Birthday_m")
  xafty_column_mix <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Arrival_Date")


  expect_equal(xafty_column_true, rep(TRUE, 6))
  expect_equal(xafty_column_false, rep(FALSE, 6))
  expect_equal(xafty_column_mix, c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))

})

test_that("Filter for type 'numeric' works", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.numeric(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no")),
                            "Arrival_Time" = as.character(c(NA, "20", "Afternoon"))
  )

  validity_table <- data.frame("Name" = "##!!factor", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!number",
                               "Arrival_Time" = "##!!number")

  xafty_column_true <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Age")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Has_Birthday")
  xafty_column_mix <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Arrival_Time")


  expect_equal(xafty_column_true, rep(TRUE, 3))
  expect_equal(xafty_column_false, rep(FALSE, 3))
  expect_equal(xafty_column_mix, c(TRUE, TRUE, FALSE))

})


test_that("Filter for type 'datetime' works", {

  check_table <- data.frame("Birthday" = as.character(c("2000-12-24 22:10", "1993-09-06 0:12",
                                                        "2022-01-01 10:10:10", "2012-09-12 12:31:59",
                                                        "12/01/1993 10:10:10", "2000/12/01 10:10:10")),
                            "Birthday_m" = as.character(c("2000-412-24 21:33", "1993/09//06 100:2:asd1",
                                                          "2022-xx01-01 21:33", "01..02.2023 21x:x33",
                                                          "12/01//1993 21:33:22", "20000/12/01 21:33:12"))
  )

  validity_table <- data.frame("Birthday_m" = "##!!datetime", "Age" = "##!!text",
                               "Birthday" = "##!!datetime", "Has_Birthday" = "##!!text",
                               "Arrival_Date" = "##!!date")

  xafty_column_true <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Birthday")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table, filter_column = "Birthday_m")
  xafty_column_false <- filter_column_type(check_table = check_table, validity_table = validity_table,
                                           filter_column = "Birthday_m", )


  expect_equal(xafty_column_true, rep(TRUE, 6))
  expect_equal(xafty_column_false, rep(FALSE, 6))

})

test_that("Function correctly retunrs error when arguments are incorrectly passed", {

  check_table <- data.frame("Name" = as.factor(c("David", "Diana", "Marcel")),
                            "Age" = as.numeric(c(22, 18, 25)),
                            "Birthday" = as.character(c("2000-12-24", "1993-09-06", "2022-01-01")),
                            "Has_Birthday" = as.character(c("no", "yes", "no"))
  )

  validity_table <- data.frame("Name" = "##!!factor", "Age" = "##!!number",
                               "Birthday" = "##!!date", "Has_Birthday" = "##!!number",
                               "Arrival_Time" = "##!!datetime"
  )

  expect_error(filter_column_type(check_table = check_table, validity_table = validity_table,
                                  filter_column = c("Arrival_Date", "Birthday")))
  expect_error(filter_column_type(check_table = check_table, validity_table = validity_table,
                                  filter_column = "Loudness"))
  expect_error(filter_column_type(check_table = check_table, validity_table = validity_table,
                                  filter_column = "Arrival_Time"))

})
