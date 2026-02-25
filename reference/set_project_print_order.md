# Set or Change the Print Order of Projects

This function allows you to manually define or update the display order
of projects within a \`xafty\` network. You can specify the print order
for individual projects or for multiple projects simultaneously by
providing a numeric vector of corresponding order values. If no order
vector is provided, each listed project will be added sequentially to
the end of the existing order.

## Usage

``` r
set_project_print_order(projects, order = NULL, network)
```

## Arguments

- projects:

  A character vector containing the names of the projects whose print
  order should be set or updated.

- order:

  A numeric vector of the same length as \`projects\`, specifying the
  desired print order for each project. If \`NULL\` (the default), each
  project will be appended to the end of the current order.

- network:

  A \`xafty\` network object containing the projects.

## Value

The updated \`xafty\` network (returned invisibly).

## Details

If a specified project already exists in the print order, its order
value will be updated. If a project is not yet part of the print order
but exists in the network, it will be added with the specified or
automatically assigned order. An error is raised if any project listed
in \`projects\` does not exist in the given network.

## Examples

``` r
xafty_network <- init_network("change_order",
                   projects = c("Project2", "Project1"))
print(xafty_network) # Prints Project2 first, then Project1
#> ---
#> 📊 change_order 
#> 
#> 🌲 Projects (2):
#>    ├📁 Project2
#>    │   └  0🌱 | 0🔗 | 0🧩 
#>    └📁 Project1
#>        └  0🌱 | 0🔗 | 0🧩 

# Change the print order
set_project_print_order(
  projects = c("Project2", "Project1"),
  order = c(2, 1),
  network = xafty_network
)
print(xafty_network)
#> ---
#> 📊 change_order 
#> 
#> 🌲 Projects (2):
#>    ├📁 Project1
#>    │   └  0🌱 | 0🔗 | 0🧩 
#>    └📁 Project2
#>        └  0🌱 | 0🔗 | 0🧩 
```
