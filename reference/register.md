# Register a Function in a Network

The function is the workhorse behind all register methods of a network.

## Usage

``` r
register(quosure, project, network, link_type, ...)
```

## Arguments

- quosure:

  A function call enquoted with rlang::enquo

- project:

  The project name of the project within the network where the function
  should be registered.

- network:

  A xafty network.

- link_type:

  The link_type name of the rulset. Currently "link" and "object" are
  supported

- ...:

  Configurations and advanced use when registering a new link

## Value

A xafty network (invisibly)
