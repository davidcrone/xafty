test_that("Function retrieves columns with xafty syntax correctly", {

  xafty_syntax <- c("##!!text", "##!!eachpattern", "##number")

    validity_table <- data.frame(
      "Plant_Name" = c("##!!text"),
      "Color" = c("##!!text", "##!!eachpattern"),
      "Petal.Size" = c("##number", "#!eachexact")
    )

  columns_xafty_pair <- obtain_columns_in_validity(validity_table = validity_table, xafty_syntax = xafty_syntax)

  expect_equal(columns_xafty_pair, c("##!!text" = "Plant_Name", "##!!text" = "Color", "##!!eachpattern" = "Color",
                                     "##number" = "Petal.Size"))

})


