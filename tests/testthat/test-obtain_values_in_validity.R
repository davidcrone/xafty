test_that("Can obtain all values under a specified rule", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = as.Date(c("2022-02-01", "2021-04-25", "2025-03-02")),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com")
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!factor", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", ".", NA)
  )

  xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = "##!!eachexact")

  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair[1]),
               c("Apple", "Banana"))
  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair[2]),
               c("2022-02-01"))

  xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = "##!!rowpattern")

  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair),
               c("Train", "Truck"))

  xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = "##!!strictpattern")

  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair),
               c("@", "."))

  xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = "##!!notempty")

  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair[1]),
               character(0))
  expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair[2]),
               character(0))

  xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = "##!!factor")

  # TODO BUG: should also return character(0)
  # expect_equal(obtain_values_in_validity(validity_table = validity_table, xafty_pair = xafty_pair),
  #              character(0))

})
