# Get the Package Name of a Function

This function determines the package where a given function is defined.

## Usage

``` r
get_function_package(func_name)
```

## Arguments

- func_name:

  A character string specifying the name of the function.

## Value

A character vector of package names where the function is found, or
\`NA\` if the function is not found in any package.
