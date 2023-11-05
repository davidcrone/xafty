test_that("as.Date_xafty can work with different date formats in a column", {
  dates <- c("2023-01-01", "01.01.2023", "01/01/2023", "2023/01/01", "01.01.2023")

  try_Format <- c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d")

  dates_converted <- as.Date_xafty(dates = dates, tryFormats = try_Format)

  dates_expectation <- as.Date(rep("2023-01-01", 5))

  expect_equal(length(dates_converted), length(dates))
  expect_equal(dates_converted, dates_expectation)
})

test_that("as.Date_xafty can work with small y Formats", {
  dates <- c("01.01.23")

  try_Format <- c("%d.%m.%y")

  dates_converted <- as.Date_xafty(dates = dates, tryFormats = try_Format)

  dates_expectation <- as.Date(rep("2023-01-01", 1))

  expect_equal(length(dates_converted), length(dates))
  expect_equal(dates_converted, dates_expectation)
})

test_that("tryFormats Parameter allows for additional formats", {
  dates <- c("2023-01-01", "01.01.2023", "01x01x2023", "01/01/2023", "2023/01/01", "01.01.2023")

  try_Format <- c("%dx%mx%Y", "%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d")

  dates_converted <- as.Date_xafty(dates = dates, tryFormats = try_Format)

  dates_expectation <- as.Date(rep("2023-01-01", 6))

  expect_equal(length(dates_converted), length(dates))
  expect_equal(dates_converted, dates_expectation)
})
