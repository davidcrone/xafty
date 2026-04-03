# Build a xafty Query

A xafty query will be passed to the nascent function in order to
retrieve data from the network

## Usage

``` r
query(...)
```

## Arguments

- ...:

  Query value. See examples for creating a simple query

## Examples

``` r
query(project_name1 = c("col1", "col2"), project_name2 = c("colA"))
#> $project_name1
#> $select
#> [1] "col1" "col2"
#> 
#> $from
#> [1] "project_name1"
#> 
#> attr(,"class")
#> [1] "list"        "xafty_query"
#> 
#> $project_name2
#> $select
#> [1] "colA"
#> 
#> $from
#> [1] "project_name2"
#> 
#> attr(,"class")
#> [1] "list"        "xafty_query"
#> 
#> attr(,"class")
#> [1] "list"             "xafty_query_list"
# Tidy-Style selection is also supported
query(var1)
#> $unevaluated
#> $select
#> [1] "var1"
#> 
#> $from
#> [1] "unevaluated"
#> 
#> attr(,"class")
#> [1] "list"      "raw_query"
#> 
#> attr(,"class")
#> [1] "list"             "xafty_query_list"
# Named vector style for renaming is also supported
query(table1 = c("Rename" = "var1"), var2)
#> $table1
#> $select
#> [1] "var1"
#> 
#> $from
#> [1] "table1"
#> 
#> $rename
#> [1] "Rename"
#> 
#> attr(,"class")
#> [1] "list"        "xafty_query"
#> 
#> $unevaluated
#> $select
#> [1] "var2"
#> 
#> $from
#> [1] "unevaluated"
#> 
#> attr(,"class")
#> [1] "list"      "raw_query"
#> 
#> attr(,"class")
#> [1] "list"             "xafty_query_list"
# Nested list style is also supported
query(list(customer_data = "id"))
#> $customer_data
#> $select
#> [1] "id"
#> 
#> $from
#> [1] "customer_data"
#> 
#> attr(,"class")
#> [1] "list"        "xafty_query"
#> 
#> attr(,"class")
#> [1] "list"             "xafty_query_list"
```
