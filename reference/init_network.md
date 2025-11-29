# Initialize a New xafty Network

Use this function to initialize a new xafty network.

## Usage

``` r
init_network(name, projects = NULL)
```

## Arguments

- name:

  Character vector of length 1. A valid name for the network

- projects:

  Character vector of project names that should be added to the network

## Value

A 'xafty_network' environment

## Examples

``` r
{
# Initialize the network
new_network <- init_network(name = "network_1")

# Add a new project
new_network$add_project("project1")
}
```
