# xafty 0.4.2

## New Features

* Context pollution now raises a warning upon registering a node that leads to a polluted context
* An error is now raised when attempting to register a node with a context that is not yet present in the network

## Bugfixes

* Fixed a bug where recursive dependency resolution for on_exit nodes would create a cycle

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
