
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xafty

<!-- badges: start -->

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
[![R CMD
Check](https://github.com/davidcrone/xafty/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidcrone/xafty/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The package is designed to help with creating automation for tasks that
are based on a spreadsheet document (like Excel). After reading in data
from a spreadsheet, values and columns usually do not align with the
assumptions of the code, especially if the document is frequently edited
by hand.

Xafty offers a lightweight solution to check whether the spreadsheet
data adheres to the specified rules stored in a validation table
(referred to as “validity table”). The package aims to achieve the
following goals:

1.  Facilitate communication between the developer and user by agreeing
    on rules detailing how the data should be structured.

2.  Enable users to repair data on their own, streamlining the
    maintenance of the automation solution.

3.  Leverage the business expertise of users to set sensible rules,
    allowing developers to make plausible assumptions about the data.

## Installation

You can install the development version of xafty like so:

``` r
remotes::install_github("davidcrone/xafty")
```

## xafty Rules

### The Check Table

The check table contains the data that will be checked. Usually, it is
data read in from a spreadsheet.

In order to explain all the possible rules and their variants, we will
use a reduced ‘mtcars’ data set as an example.

``` r
data(mtcars)
mtcars$name <- rownames(mtcars)
rownames(mtcars) <- NULL
mtcars <- mtcars[1:5, c("name", "cyl", "wt")]
mtcars$cyl <- factor(mtcars$cyl)

mtcars
```

### The Validity Table

A validity table consists of column names and column rules that will
then be compared to the corresponding columns in the check table.

To start of building the validity table we set the column names without
any rules:

``` r
# NA values will always be ignored in a validity table
validity_table <- data.frame("name" = NA, "cyl" = NA, "wt" = NA)
```

### Check Number of Columns

Checks if the number (count) of columns of the check table is equal to
that of the validity table. Use this check if the number of columns is
important for downstream code.

**Rule syntax:** The number of columns in the validity table

``` r
check_column_number(check_table = mtcars, validity_table = validity_table, check_type = "equal")
```

### Check Column Names

Checks if the columns in the check table are all correctly named and
present. There exists a stricter variant of the check by specifying the
check type as “order” which additionally checks if the columns in the
check table are in the correct order as specified in the validity table.

**Rule syntax:** The name and order of the columns in the validity table

``` r
check_column_names(check_table = mtcars, validity_table = validity_table, check_type = "presence")
```

### Check Column Data Types

Checks whether all columns in the check table have the correct data type
(class) associated with them. To perform the check the validity table
must have data type rules under each column.

**Rule syntax:**

| Rule          | Data Type |
|---------------|-----------|
| \##!!number   | integer   |
| \##!!number   | double    |
| \##!!text     | character |
| \##!!factor   | factor    |
| \##!!date     | Date      |
| \##!!datetime | POSIXct   |

``` r
validity_table <- data.frame("name" = "##!!text", "cyl" = "##!!factor", "wt" = "##!!number")
check_column_types(check_table = mtcars, validity_table = validity_table)
```

### Check for Empty Values

Checks for empty (NA) column values. Use this check when a column should
not have any NA values. Consequently, NA values must be correctly
identified before being able to use this check correctly.

**Rule syntax:** \##!!notempty

``` r
validity_table <- data.frame("name" = c("##!!text", "##!!notempty"), "cyl" = c("##!!factor", "##!!notempty"), "wt" = c("##!!number", "##!!notempty"))
check_column_notempty(check_table = mtcars, validity_table = validity_table)
```

### Check for Exact Values

Checks whether the provided values in the validity table match exactly
the data in the check table. To provide values to match for in the
validity, we simply add the desired rule followed by the values the
algorithm should look for.

**Rule syntax:**

| Rule             | Explanation                                                                                       |
|------------------|---------------------------------------------------------------------------------------------------|
| \##!!strictexact | All values in the check table must match the provided values in the validity table                |
| \##!!anyexact    | At least one value in the check table must match any of the provided values in the validity table |
| \##!!eachexact   | All provided values in the validity table must be matched at least once in the check table        |

``` r
validity_table <- data.frame("name" = c("##!!text", "##!!notempty", "##!!anyexact", "Mazda RX4"), 
                             "cyl" = c("##!!factor", "##!!notempty", NA, NA), 
                             "wt" = c("##!!number", "##!!notempty", NA, NA))
check_column_exactinput(check_table = mtcars, validity_table = validity_table)
```

### Check for Pattern Values

Checks whether the provided patterns in the validity table match the
data in the check table. Several variant rules exist for this check.

**Rule syntax:**

| Rule               | Explanation                                                                                            |
|--------------------|--------------------------------------------------------------------------------------------------------|
| \##!!strictpattern | All values in the check table must match the provided patterns in the validity table                   |
| \##!!rowpattern    | Every row of the column in the check table must match to at least to one pattern in the validity table |
| \##!!anypattern    | At least one value in the check table must match to a pattern in the validity table                    |
| \##!!eachpattern   | Every pattern provided in the validity table must match at least once in the check table               |

``` r
validity_table <- data.frame("name" = c("##!!text", "##!!notempty", "##!!rowpattern", "Mazda", "Datsun", "Hornet"), 
                             "cyl" = c("##!!factor", "##!!notempty", NA, NA, NA, NA), 
                             "wt" = c("##!!number", "##!!notempty", NA, NA, NA, NA))
check_column_patterninput(check_table = mtcars, validity_table = validity_table)
```

## Example

Basic workflow to check a table for its ‘validity’ before using it in
downstream code:

``` r
library(xafty)

# Load example data
data <- read_example_data() 
check_table <- data$check_table
validity_table <- data$validity_table

# Aligns the data type of the check table as specified in the validity table
check_table <- align_column_types(check_table = check_table, validity_table = validity_table)

# Check if the check table is valid
check_result <- check_validity(check_table = check_table, validity_table = validity_table)

if (all(check_result$Check_Result)) {

    print("Everythin is in order. 'Proceed with automation task.'")
    
   } else {
   
    print("Check table does not adhere to specifications. 'Proceed with repairing actions.'")
   
   }
```