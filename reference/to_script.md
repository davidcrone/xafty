# Write a Pipeline to a Script

This function is used in conjunction with build_dag() to write the
created pipeline as a linear script. This is useful for debugging
purposes to see on which step the pipeline fails. The function creates a
single file which can be run from top to bottom as it would be evaluated
in nascent()

## Usage

``` r
to_script(dag, file = NULL)
```

## Arguments

- dag:

  A directed acyclic graph object obtained by build_dag()

- file:

  An R-script file to write to

## Value

TRUE (invisibly)
