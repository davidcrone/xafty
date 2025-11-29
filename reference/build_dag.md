# Build an 'Evaluable' Data Pipeline Object

When querying an object, the xafty algorithm recursively iterates
through the network and obtains all functions necessary. Before
evaluating all functions, the xafty algorithm creates a dag-object which
contains the full information about dependencies. The object can then be
evaluated with function: evaluate_dag

## Usage

``` r
build_dag(..., network, frame = "main")
```

## Arguments

- ...:

  A xafty query list object

- network:

  A xafty network

- frame:

  Used for debugging

## Value

A list
