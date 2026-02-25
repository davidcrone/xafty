# Add a State to a xafty Query

The state of a xafty query is passed to arguments that were declared as
xafty states by passing a single character vector wrapped into
`{curley_braces}` into the argument

## Usage

``` r
with_state(query_list, ...)
```

## Arguments

- query_list:

  A query list created by \[xafty::query\]

- ...:

  Declaration of the states e.g. state_name = TRUE

## Value

A list state_query which is a query bundled with a state
