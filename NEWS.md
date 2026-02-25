# xafty 0.4.0

## New Features

* Projects now have the new function `add_context()` which allows to create a context which can be 'attached' to nodes
* Nodes now can be linked with an attached context using the added parameter `attach_context` when linking/updating a new node to a project
* Networks and projects now print differently giving adequate summary statistics for each
* One-directional joins are now supported and set as the default. Joinen Bi-directional is now possible when setting the parameter `direction` to 'both' when linking a join
