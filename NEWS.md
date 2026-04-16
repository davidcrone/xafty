# xafty 0.4.3

## New Feature

* Project names with a dot are now not allowed and creating a project with a dot, will raise an error

# xafty 0.4.3

## New Feature

* Renaming variables in a query is now possible and works seamlessly, simply query(table = c("renamed" = "name")) 
  to rename your variable
* Added cycle detection. Upon registering a node that would result in a cycle, the register is reverted as well as
  providing an informative error on the detected cycle
  
# xafty 0.4.2

## New Features

* Printing a project now also will print variables sorted into groups using the group parameter when linking a node
* An error is now raised upon registering a new node that adds the same variable names to the project
* An error is now raised when attempting to register a state named 'xafty_global_default'
* A warning is now raised upon registering a node that creates to a polluted context
* A warning is now raised when attempting to register a node with a context that is not yet present in the network

## Bugfixes

* Fixed a bug where recursive dependency resolution for on_exit nodes would create a cycle
* Fixed a bug where {.data} would create duplicated column names in the data.frame during evaluation phase

# xafty 0.4.1

## New Features

* Printing a network now shows available states
* Printing a project now shows all available variables through direct and indirect joins

# xafty 0.4.0

## New Features

* Projects now have the new function `add_context()` which allows to create a context which can be 'attached' to nodes
* Nodes now can be linked with an attached context using the added parameter `attach_context` when linking/updating a new node to a project
* Networks and projects now print differently giving adequate summary statistics for each
* One-directional joins are now supported and set as the default. Joining bi-directional is now done by setting the parameter `direction` to 'both' upon linking the join
