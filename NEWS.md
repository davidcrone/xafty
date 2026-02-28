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
