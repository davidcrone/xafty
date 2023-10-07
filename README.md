
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xafty

<!-- badges: start -->

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
<!-- badges: end -->

The package is designed to facilitate task automation for tasks that are
often completed in Excel. After reading in data from Excel, values and
columns usually do not align with the assumptions of the code. The xafty
package offers a lightweight solution to check whether the Excel data
adheres to the specified rules stored in a validation table (referred to
as “validity table”). The package aims to achieve the following goals:

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

## xafty Rules (WIP)

### Check Number of Columns

### Check Column Names

### Check Column Data Types

### Check for Empty Values

### Check for Exact Values

### Check for Pattern Values

## Example

Basic workflow to check a table for its ‘validity’ before using it in
downstream code:

``` r
library(xafty)

# Load example data
data <- read_check_and_validity() 
check_table <- data$check_table
validity_table <- data$validity_table

# (Optional) Aligns the data type of the check table as specified in the validity table
check_table <- align_column_types(check_table = check_table, validity_table = validity_table)

# Check if the check table is valid
check_result <- check_validity(check_table = check_table, validity_table = validity_table)

if (all(check_result$Check_Result)) {

    print("Everythin is in order. 'Proceed with automation task.'")
    
   } else {
   
    print("Check table does not adhere to specifications. 'Proceed with repairing actions.'")
   
   }
```
