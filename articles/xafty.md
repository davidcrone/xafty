# Introduction to xafty

The goal of xafty is to build R-based pipelines that are reusable,
scalable, and easy to collaborate on. To achieve this end, xafty
represents your pipeline as a directed acyclic graph (DAG) where each
step becomes a node in a network.

This changes how you work with pipelines in one significant way: instead
of executing steps in a predetermined sequence, xafty dynamically
constructs the pipeline at run time based on the data you request.

This article gives you an intro to building network-pipelines. If you’ve
already put together pipelines in R using base R or dplyr, you’ve got
everything you need to start building them with xafty.

## Setup

``` r
library(xafty)
```

To create a network object, we call
[`xafty::init_network()`](https://davidcrone.github.io/xafty/reference/init_network.md).

``` r
network <- init_network("intro_network")
```

Each network object must have at least one project in order to start
building the network pipeline.

A project can be added as following:

``` r
network$add_project("mtcars")
```

Projects are multi-purpose structures which are used to hold and
organize your nodes, as well as encapsulate logic and settings how nodes
should be treated within their respective projects.

This is all the setup we need to start building our network. From here,
the process is just like creating any other pipeline: we need data and
the transformation steps that operate on it.

## Building a Network Pipeline

Let’s add the mtcars dataset to our network.

To do that, we write a function that returns the data.frame:

``` r
get_mtcars <- function() {
  data("mtcars", envir = environment())
  mtcars
}

head(get_mtcars())
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

To start building a network pipeline, we register the function into the
`mtcars` project:

``` r
network$mtcars$get(get_mtcars())
```

Since the function `get_mtcars` has no dependencies, it is automatically
set as the root node of the project. **Note:** currently, xafty expects
a project to have only one or no root node.

We can inspect the project by printing it to the console and query the
network for our desired variables:

``` r
# Inspecting the network
network
#> Number of projects in network 'intro_network': 1
#> 
#> Project: mtcars
#> Variables: drat, vs, hp, am, disp, cyl, carb, mpg, qsec, wt, gear

# Wildcard selection returns all variables of a project
mtcars <- nascent(network, query(mtcars = "*"))

# Querying variables by name will only return the desired variables
data <- nascent(network, query(mtcars = c("carb", "gear")))
head(data)
#>                   carb gear
#> Mazda RX4            4    4
#> Mazda RX4 Wag        4    4
#> Datsun 710           1    4
#> Hornet 4 Drive       1    3
#> Hornet Sportabout    2    3
#> Valiant              1    3
```

### Adding a Node

To let our network grow, we simply query the data we need from the
network, write a step as a function that transforms the data, and
register the function again into the network:

``` r

# 1. retrieve data from the network
data <- nascent(network, query(mtcars = c("hp", "wt")))

# 2. write the transformation step
add_power_to_weight <- function(data) {
  data$power_to_weight <- data$hp / data$wt
  data
}

# 3. register the step to the network
network$mtcars$add(add_power_to_weight(data = query(mtcars = c("hp", "wt"))))
```

When registering a transformation step in the network, we do not pass
data directly to the function. Instead, we declare the required inputs
using
[`xafty::query`](https://davidcrone.github.io/xafty/reference/query.md).
Internally, this query is resolved by
[`xafty::nascent()`](https://davidcrone.github.io/xafty/reference/nascent.md)
to obtain the actual data and evaluate the function’s return value, so
the network can determine which variables it contributes.

To avoid recalculating the entire data pipeline, you can register the
function by explicitly stating which variables it adds to the network
using the parameter `vars`:

``` r
network$mtcars$add(fun = add_power_to_weight(data = query(mtcars = c("hp", "wt"))), 
                   vars = "power_to_weight", update = TRUE)
# Note: Setting update = TRUE prevents the network from asking
# whether an already registered function should be updated.
```

Each transformation step (e.g. the function `add_power_to_weight`) in a
xafty network is written as a **“pass-through”** function. Such a
function must satisfy the following requirements:

- It must refer to variables by name, as column positions cannot be
  guaranteed. (If your function depends on a specific data structure
  rather than just variable names, you should use `objects` instead.)
- It must leave the input `data.frame` unchanged and return the entire
  `data.frame` with the added transformation (as embodied by
  [`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)).

These are necessary conditions that make network pipelines possible and
connect to a larger vision:

In a network pipeline, each node represents a self-contained
transformation that encapsulates knowledge about how to derive specific
outputs from given inputs. Rather than being a temporary, task-specific
function, a node defines a reusable mapping between data states, forming
a building block in the network’s directed acyclic graph. As new
patterns in the data emerge and domain knowledge continues to grow,
nodes will be refined and updated. I envision network pipelines as a
continuously evolving data structures that captures and preserves all
the domain knowledge a data analyst can express in code.

### Adding a Join

To join two data sources, we first need to create a second project with
another root node:

``` r
network$add_project("engine")

get_engine_details <- function() {
  engine <- data.frame(
    type = as.factor(c("Straight", "V-Shape")),
    vs = c(1, 0)
  )
  engine
}

network$engine$get(get_engine_details())
```

The join is then added to the network as another transformation step. We
tell the network that we wish to register a join by passing two separate
query-calls to the function:

``` r
join_engine_details <- function(mtcars, engine) {
  joined <- merge(mtcars, engine, by = "vs", all.x = TRUE, sort = FALSE)
  joined
}

network$engine$join(join_engine_details(mtcars = query(mtcars = "vs"),
                                        engine = query(engine = "vs")))
```

At the moment, joins are treated as symmetrical operations. As a result,
the network supports only one join definition for each pair of projects.
Moreover, the network assumes that the join is performed between the
projects that appear first in the corresponding
[`xafty::query()`](https://davidcrone.github.io/xafty/reference/query.md)
call.

To declare that a function depends on a join, register it using a single
[`xafty::query()`](https://davidcrone.github.io/xafty/reference/query.md)
call that includes at least two different projects with root nodes:

``` r
add_combined_label <- function(data) {
  data$combined_label <- paste(
     data$type, 
     ifelse(data$am == 0, "Auto", "Manual"),
      sep = "_"
  )
  data
}

network$mtcars$add(add_combined_label(data = query(mtcars = "am", engine = "type")))
```

### Grouped Operations

In many workflows, we temporarily group data so that downstream
transformations can operate with group-level context. In a typical dplyr
pipeline, this is as simple as calling group_by(), performing the
transformation, and then calling ungroup().

In a xafty network pipeline, however, grouping must be declared
explicitly. Because nodes are independent and assembled dynamically,
grouping cannot be embedded inline within each function. Instead, xafty
lets you define grouped execution contexts at the project level using
**entry** and **exit** hooks.

A project can automatically group data when a node is entered and
ungroup it when the node finishes, allowing transformation functions to
assume grouped input without containing grouping logic themselves.

We start by defining three pass-through functions:

``` r
library(dplyr, warn.conflicts = FALSE)

group_by_gear <- function(data) {
  group_by(data, gear)
}

ungroup_data <- function(data) {
  ungroup(data)
}

add_mean_hp_per_gear <- function(data) {
  mutate(data, mean_hp_per_gear = mean(hp))
}
```

Next, we create a new project that we will use to attach the context to:

``` r
network$add_project("per_gear")
```

We tell the project to apply grouping whenever data enters a node:

``` r
network$per_gear$on_entry(group_by_gear(data = query(mtcars = "gear")))
```

And to remove grouping once a node completes its computation:

``` r
network$per_gear$on_exit(ungroup_data(data = "{.data}"))
```

Now, all nodes registered in `per_gear` will inherit this grouping
behavior when they run.

``` r
network$per_gear$add(add_mean_hp_per_gear(data = query(mtcars = "hp")))

data <- nascent(network, query(mtcars = c("gear"), per_gear = "mean_hp_per_gear"))

head(data)
#> # A tibble: 6 × 2
#>    gear mean_hp_per_gear
#>   <dbl>            <dbl>
#> 1     4             89.5
#> 2     4             89.5
#> 3     4             89.5
#> 4     3            176. 
#> 5     3            176. 
#> 6     3            176.
```

A project may have multiple `on_entry()` or `on_exit()` functions. All
entry and exit hooks are applied in the order they were registered. This
allows us to layer context-setting operations, such as grouping,
arranging, or reshaping, without embedding them directly into individual
transformation functions.

### Inspecting Pipelines

As your network grows, it becomes increasingly important to understand
how xafty assembles and executes your pipeline. Since nodes are combined
dynamically based on the variables you request, xafty provides tools to
inspect the resulting execution plan before running it.

The function
[`build_dag()`](https://davidcrone.github.io/xafty/reference/build_dag.md)
constructs the directed acyclic graph for a given query and returns a
list-object that describes the full pipeline:

``` r
  dag <- build_dag(query(mtcars = c("gear"), per_gear = "mean_hp_per_gear", engine = "type"), network = network)
  dag$execution_order
#> [1] "mtcars.get_mtcars"             "engine.get_engine_details"    
#> [3] "engine.join_engine_details"    "per_gear.group_by_gear"       
#> [5] "per_gear.add_mean_hp_per_gear" "per_gear.ungroup_data"
```

## Objects

Explaining objects

## States

Programming with network pipelines

### Defining a Join Path
