test_that("Xafty List shows all rules as true if all rules are fullfilled", {
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

  check_table <- align_column_types(check_table = check_table, validity_table = validity_table)

  xafty_list <- build_xafty_list(check_table = check_table, validity_table = validity_table, xafty_rules_table = xafty_rules_table)

  summary_test_table <- build_xafty_test_table(xafty_list = xafty_list)

  expect_true(all(summary_test_table$test_result))
  expect_equal(summary_test_table$column, c(
    "Product_Name", "Product_Name", "Product_Name", "Product_Weight",
    "Expiration_Date", "Expiration_Date", "Delivery_Time", "Is_Delivered_By",
    "Is_Delivered_By", "Is_Delivered_By", "Mail_Customer", "Mail_Customer"
  ))
  expect_equal(summary_test_table$rule, c(
    "##!!notempty", "##!!eachexact", "##!!text", "##!!number", "##!!eachexact",
    "##!!date", "##!!datetime", "##!!notempty", "##!!factor", "##!!rowpattern",
    "##!!text", "##!!strictpattern"
  ))
})
