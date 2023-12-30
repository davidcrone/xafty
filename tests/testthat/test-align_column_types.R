test_that("Align column types changes columns to the specified type in the validity table", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c("2.1", "0.5", "1.0"),
    "Expiration_Date" = c("2022-02-01", "2021-04-25", "2025-03-02"),
    "Delivery_Time" = c("2023-09-12 22:12:01", "2023-09-17 22:12:01", "2023-09-12 01:12:01"),
    "Is_Delivered_By" = c("zzz__Train", "Train no Rails", "Truck"),
    "Mail_Customer" = c("applelover@yahoo.com", "banana_digester@bananas.uk", "grrm@asoiaf.com"),
    "Date_Arrival" = c("102203", "102202", "102201"),
    "W123" = c("1", "1", "1"),
    "W421" = c("1", "0", "0"),
    "T333" = c("1.1", "2", "0")
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

  validity_table <- add_regex_columns_to_validity(check_table, validity_table)

  check_table_aligned <- align_column_types(check_table, validity_table, force_type = TRUE)

  column_types <- do.call(c ,sapply(check_table_aligned, class, simplify = TRUE))
  names(column_types) <- NULL

  expect_equal(column_types, c("character", "numeric", "Date", "POSIXct", "POSIXt", "character", "character", "Date",
                               "numeric", "numeric", "numeric"))

})

test_that("align_column_types does not coerce values to NA if force_type is FALSE", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c("2.1", "a0.5", "1.0"),
    "Expiration_Date" = c("2022.02-01", "2021-04-25", "2025-03-02"),
    "Delivery_Time" = c("2023-09-12 22:12:01", "2023-09.17 22:12:01", "2023-09-12 01:12:01")
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!factor", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime")
  )

  check_table_aligned <- align_column_types(check_table, validity_table, force_type = FALSE)

  column_types <- sapply(check_table_aligned, class)
  names(column_types) <- NULL

  expect_equal(column_types, c("factor", "character", "character", "character"))
  expect_equal(check_table$Product_Weight, check_table_aligned$Product_Weight)
  expect_equal(check_table$Expiration_Date, check_table_aligned$Expiration_Date)
  expect_equal(check_table$Delivery_Time, check_table_aligned$Delivery_Time)

})

test_that("align_column_types does coerce values to NA if force_type is TRUE", {

  check_table <- data.frame(
    "Product_Name" = c("Apple", "Banana", "Apple"),
    "Product_Weight" = c("2.1", "a0.5", "1.0"),
    "Expiration_Date" = c("2022.02-01", "2021-04-25", "2025-03-02"),
    "Delivery_Time" = c("2023-09-12 22:12:01", "2023-09.17 22:12:01", "2023-09-12 01:12:01")
  )

  validity_table <- data.frame(
    "Product_Name" = c("##!!factor", "##!!eachexact", "Apple", "Banana", "##!!notempty"),
    "Product_Weight" = c("##!!number", NA, NA, NA, NA),
    "Expiration_Date" = c("##!!date", "##!!eachexact", "2022-02-01", NA, NA),
    "Delivery_Time" = c("##!!datetime")
  )

  check_table_aligned <- suppressWarnings(align_column_types(check_table, validity_table, force_type = TRUE))

  column_types <- do.call(c, sapply(check_table_aligned, class))
  names(column_types) <- NULL

  expect_equal(column_types, c("factor", "numeric", "Date", "POSIXct", "POSIXt"))
  expect_equal(as.character(check_table_aligned$Product_Name), check_table$Product_Name)
  expect_equal(as.character(check_table_aligned$Product_Weight), c("2.1", NA, "1"))
  expect_equal(as.character(check_table_aligned$Expiration_Date), c(NA, "2021-04-25", "2025-03-02"))
  expect_equal(as.character(check_table_aligned$Delivery_Time), c("2023-09-12 22:12:01", NA, "2023-09-12 01:12:01"))

})
