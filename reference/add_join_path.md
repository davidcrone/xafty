# Add a Predefined Join Path to a Query

The function gives control over the desired joins in order to bring all
projects together into one data.frame. The joins are read left-to-right.
An example can be found under examples on how to structure the list.

## Usage

``` r
add_join_path(query_list, ...)
```

## Arguments

- query_list:

  A xafty query list returned by query()

- ...:

  List of the join path

## Value

A list

## Examples

``` r
if (FALSE) { # \dontrun{
qry <- query(projectA = "col1", projectB = "col2", projectC = "col3")
add_join_path(qry, path1 = c("projectA", "projectB"), path2 = c("projectA", "projectC"))
# this creates the following joins: projectA joins with projectB and projectA joins with projectC
} # }
```
