test_that("Xafty List shows all rules as true if all rules are fullfilled", {
  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = as.Date(c("2022-02-01", "2021-04-25", "2025-03-02")),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com"),
    "Date_Arrival" = c("102203", "102202", "102201")
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!factor", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", ".", NA),
    "Date_Arrival" = c("##!!date", NA, NA, NA, NA)
  )

  check_table <- align_column_types(check_table = check_table, validity_table = validity_table)

  xafty_list <- build_xafty_list(check_table = check_table, validity_table = validity_table,
                                check_names = FALSE, check_number = FALSE)

  summary_test_table <- build_xafty_test_table(xafty_list = xafty_list)

  expect_true(all(summary_test_table$test_result))
  expect_equal(summary_test_table$column, c(
    "Product_Name", "Product_Name", "Product_Name", "Product_Weight",
    "Expiration_Date", "Expiration_Date", "Delivery_Time", "Is_Delivered_By",
    "Is_Delivered_By", "Is_Delivered_By", "Mail_Customer", "Mail_Customer",
    "Date_Arrival"
  ))
  expect_equal(summary_test_table$rule, c(
    "##!!notempty", "##!!eachexact", "##!!text", "##!!number", "##!!eachexact",
    "##!!date", "##!!datetime", "##!!notempty", "##!!factor", "##!!rowpattern",
    "##!!text", "##!!strictpattern", "##!!date"
  ))
})

test_that("Regex columns is supported in xafty list function", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = as.Date(c("2022-02-01", "2021-04-25", "2025-03-02")),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com"),
    "Date_Arrival" = c("102203", "102202", "102201"),
    "W123" = c(1, 1, 1),
    "W421" = c(1, 0, 0),
    "T333" = c(1.1, 2, 0)
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!text", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", NA, NA),
    "Date_Arrival" = c("##!!date", NA, NA, NA, NA),
    "Wagon_Design" = c("##!!number", "##!!regexcolumns", "^W[1-9]", "333", "##!!notempty")
  )

  check_table <- align_column_types(check_table = check_table, validity_table = validity_table)

  xafty_list <- build_xafty_list(check_table = check_table, validity_table = validity_table)

  summary_xafty_table <- build_xafty_test_table(xafty_list = xafty_list)$test_result

  expect_true(all(summary_xafty_table))
  expect_equal(length(summary_xafty_table), 24)
})

test_that("Xafty_list can filter bad dates in a data set", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = c("2022-02-01", "2021.04-25", "2025-03-02"),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com"),
    "Date_Arrival" = c("102203", "102202", "102201"),
    "W123" = c(1, 1, 1),
    "W421" = c(1, 0, 0),
    "T333" = c(1.1, 2, 0)
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!text", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", ".", NA),
    "Date_Arrival" = c("##!!date", NA, NA, NA, NA),
    "Wagon_Design" = c("##!!number", "##!!regexcolumns", "^W[1-9]", "333", "##!!notempty")
  )

  xafty_list <- build_xafty_list(check_table = check_table, validity_table = validity_table)
  expect_equal(check_table$Expiration_Date[xafty_list$Expiration_Date$date$filter_result], "2021.04-25")
})

test_that("Column missing in check_table works with build_xafty_list", {

  check_table <- data.frame(
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = c("2022-02-01", "2021.04-25", "2025-03-02"),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com"),
    "Date_Arrival" = c("102203", "102202", "102201"),
    "W123" = c(1, 1, 1),
    "W421" = c(1, 0, 0),
    "T333" = c(1.1, 2, 0)
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!text", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", "e", NA),
    "Date_Arrival" = c("##!!date", NA, NA, NA, NA),
    "Wagon_Design" = c("##!!number", "##!!regexcolumns", "^W[1-9]", "333", "##!!notempty")
  )

  xafty_list <- build_xafty_list(check_table = check_table, validity_table = validity_table)

  expect_equal(colnames(validity_table)[xafty_list$meta_tests$xafty_column_names$filter_result$colnames_validity_table],
               "Product_Name")
})
