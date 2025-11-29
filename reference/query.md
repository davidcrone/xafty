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
```
