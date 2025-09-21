
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xafty

<!-- badges: start -->
<!-- badges: end -->

**xafty** lets you build data pipelines as a directed acyclic graph
(DAG), turning each step into a reusable node in a knowledge network.
This makes pipelines more scalable, easier to collaborate on, and less
complex over time through layered abstraction.

⚠️ **Note:** In its current iteration, xafty should be understood as a
proof of concept. The design and functionality are still evolving and
may change significantly in future versions.

## Installation

``` r
 pak::pkg_install("davidcrone/xafty")
```

## Basic Functionality

``` r

########################
## START: Preperation ##
########################

library(xafty)

## Build your data pipeline one step at time
get_mtcars <- function() {
  data("mtcars", envir = environment())
  mtcars
}

add_power_to_weight <- function(mtcars) {
  mtcars$power_to_weight <- mtcars$hp / mtcars$wt
  mtcars
}

get_engine_details <- function() {
  engine <- data.frame(
    type = c("Straight", "V-Shape"),
    vs = c(1, 0)
  )
  engine
}

join_engine_details <- function(mtcars, engine) {
  merge(mtcars, engine, all.x = TRUE, sort = FALSE)
}

######################
## END: Preperation ##
######################

## Initialize the network
xafty_network <- init_network("example_network")

## Register the functions in project 1
xafty_network$add_project("mtcars")
xafty_network$mtcars$get(get_mtcars())
xafty_network$mtcars$add(add_power_to_weight(mtcars = query(mtcars = c("hp", "wt"))))

## Register the function in project 2
xafty_network$add_project("engine")
xafty_network$engine$get(get_engine_details())

# Join the two projects together
xafty_network$mtcars$join(join_engine_details(mtcars = query(mtcars = "vs"),
                                              engine = query(engine = "vs")))

# Print the network
xafty_network

# Pull data from the network
xafty_network |> nascent(mtcars = c("hp", "wt", "vs"), engine = "type", mtcars = "power_to_weight")
 
```

## Get Involved

**xafty** is in active development, and your feedback is incredibly
valuable.  
If you’re curious, testing it out, or have ideas to improve it [get in
touch](mailto:davidjvcrone@gmail.com) or [open an
issue](https://github.com/davidcrone/xafty/issues). Whether it’s bugs,
feature requests, or just to chat, I’d love to hear from you! :)
