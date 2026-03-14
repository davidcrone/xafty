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

  Configurations and advanced use when registering a new link. Notable
  options: - \`group\` (character): Assign the resulting variables to an
  organizing group for printing purposes. If the group doesn't exist, it
  will be created automatically. Updating the node with group set to
  NULL will remove the variables from any previously assigned group. -
  \`attach_context\` (character): Attach the node to a context
  wrapper. - \`update\` (logical): Whether to update if the function is
  already registered. - \`direction\` (character): For joins, "one" or
  "both" for bidirectional registration. - \`vars\` (character vector):
  Explicitly specify output variable names.

## Value

A xafty network (invisibly)
