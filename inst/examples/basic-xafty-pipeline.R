
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

## Initialize the network
xafty_network <- init_network()

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
