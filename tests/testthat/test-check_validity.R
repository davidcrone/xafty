test_that("Main functionality works as expected when all rules are fullfilled", {
  check_table <- data.frame(
    "Product_ID" = c("345", "341", "441"),
    "Product_Name" = c("Apple", "Banana", "Pencil"),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = as.Date(c("2022-02-01", "2021-04-25", "2025-03-02")),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com")
  )

  validity_table <- data.frame(
    "Product_ID" = c("##!!text"),
    "Product_Name" = c("##!!text", "##!!eachexact", "Apple", NA, NA),
    "Product_Weight" = c("##!!number"),
    "Expiration_Date" = c("##!!date"),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!factor", "##!!rowpattern", "Train", "Truck", "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", ".", "##!!notempty")
  )

  check_result <- check_validity(
    check_table = check_table, validity_table = validity_table,
    column_number = TRUE, column_names = "presence", column_types = TRUE,
    values_notempty = TRUE, values_exact = TRUE, values_pattern = TRUE
  )

  expect_true(all(check_result$Check_Result))
  expect_true(all(is.na(check_result$Columns)))
})


test_that("Main functionality works as expected when all rules are broken", {
  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", NA),
    "Product_Weight" = c(2.1, 0.5, 1.0),
    "Expiration_Date" = as.Date(c("2022-02-01", "2021-04-25", "2025-03-02")),
    "Delivery_Time" = as.POSIXct(c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01")),
    "Is_Delivered_By" = factor(c("zzz__Train", "Train no Rails", "Truck")),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com")
  )

  validity_table <- data.frame(
    "Product_ID" = c("##!!date"),
    "Product_Name" = c("##!!date", "##!!eachexact", "Apple", "Syrup", "##!!notempty"),
    "Product_Weight" = c("##!!number", "##!!strictexact", "1", "2", "3"),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-03-01", NA, NA),
    "Delivery_Time" = c("##!!datetime"),
    "Is_Delivered_By" = c("##!!factor", "##!!rowpattern", "Train", NA, "##!!notempty"),
    "Mail_Customer" = c("##!!text", "##!!strictpattern", "@", NA, "xx")
  )

  check_result <- check_validity(
    check_table = check_table, validity_table = validity_table,
    column_number = TRUE, column_names = "presence", column_types = TRUE,
    values_notempty = TRUE, values_exact = TRUE, values_pattern = TRUE
  )

  expect_false(any(check_result$Check_Result))
  expect_equal(check_result$Columns[1], as.character(NA))
  expect_equal(check_result$Columns[2], "Product_ID")
  expect_equal(check_result$Columns[3], "Product_Name")
  expect_equal(check_result$Columns[4], "Product_Name")
  expect_equal(check_result$Columns[5], c("Product_Weight, Product_Name, Expiration_Date"))
  expect_equal(check_result$Columns[6], c("Mail_Customer, Is_Delivered_By"))
})
