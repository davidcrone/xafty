test_get_car_data <- function(conn) {
  if(conn) {
    data <- data.frame(
      "Name" = c("David", "Diana", "Marcel"),
      "Car" = c("Mazda", "Renault", "BMW"),
      "Has_Drivers_License" = c("Yes", "No", "Yes")
    )
  } else {
    data <- NULL
  }
  data
}

test_add_car_color <- function(data) {
  data$Car_Color <- c("Silver", "Black", "Blue_Grey")
  data
}

setup_table <- data.frame(
    "Name" = c("David", "Diana", "Marcel"),
    "Car" = c("Mazda", "Renault", "BMW"),
    "Has_Drivers_License" = c("Yes", "No", "Yes")
)

test_get_car_color_id <- function() {
  setup_car_id <- data.frame(
    "Car_Color" = c("Silver", "Black"),
    "ID" = c("12", "34")
  )
  setup_car_id
}

test_join_car_id <- function(data_left, data_right) {
  base::merge(data_left, data_right, by = intersect(names(data_left), names(data_right)), all.x = TRUE, sort = FALSE)
}

xafty_network <- init_network("xafty_network")
xafty_network$add_project("test1")

xafty_network$test1$get(test_get_car_data(conn = TRUE))
xafty_network$test1$add(test_add_car_color(data = query(test1 = c("Has_Drivers_License", "Name", "Car"))))

xafty_network$add_project("test2")
xafty_network$test2$get(test_get_car_color_id())

xafty_network$test1$join(fun = test_join_car_id(data_right = query(test2 = "Car_Color"), data_left = query(test1 = "Car_Color")))

# setup tornado

get_main_tornado <- function() {
  data <- data.frame(id = c(1, 2, 3))
  data
}

get_join_1 <- function() {
  data <- data.frame(id = c(1, 2, 3),
                     col1 = c("Why", "What", "How"))
  data
}

get_join_2 <- function() {
  data <- data.frame(id = c(1, 2, 3),
                     col2 = c("Hallo", "Ja", "Nein"))
  data
}

get_join_3 <- function() {
  data <- data.frame(id = c(1, 2, 3),
                     col3 = c("Bonjour", "Salut", "Ca va"))
  data
}


join_main1 <- function(main, join1) {
  base::merge(main, join1, by = intersect(names(main), names(join1)), all.x = TRUE, sort = FALSE)
}

join_main2 <- function(main, join2) {
  base::merge(main, join2, by = intersect(names(main), names(join2)), all.x = TRUE, sort = FALSE)
}

join_main3 <- function(main, join3) {
  base::merge(main, join3, by = intersect(names(main), names(join3)), all.x = TRUE, sort = FALSE)
}

main_network <- init_network("main_network")
main_network$add_project("main")
main_network$add_project("side1")
main_network$add_project("side2")
main_network$add_project("side3")

# register all get functions
main_network$main$get(get_main_tornado())
main_network$side1$get(get_join_1())
main_network$side2$get(get_join_2())
main_network$side3$get(get_join_3())

# join all side projects with main
main_network$main$join(join_main1(main = query(main = "id"), join1 = query(side1 = "id")))
main_network$main$join(join_main2(main = query(main = "id"), join2 = query(side2 = "id")))
main_network$main$join(join_main3(main = query(main = "id"), join3 = query(side3 = "id")))

data_tornado <- main_network |> nascent(query(side1 = "col1", side2 = "col2", side3 = "col3"))

# Build simple wrapper

reorder_cars_by_color <- function(cars) {
  cars <- cars[order(cars$Car_Color), ]
  cars
}

add_tries_data_license <- function(data) {
  data_license <- data.frame(
    Name = c("David", "Diana", "Marcel"),
    Tries = c(5L, 1L, 2L)
    )
  data <- merge(data, data_license, all.x = TRUE, sort = FALSE)
  data
}

wrapper_network <- init_network("wrapper", projects = c("cars", "group"))
wrapper_network$cars$get(test_get_car_data(conn = TRUE))
wrapper_network$cars$add(test_add_car_color(data = query(cars = c("Has_Drivers_License", "Name", "Car"))))
wrapper_network$group$on_entry(reorder_cars_by_color(cars = query(cars = "Car_Color")))
wrapper_network$group$add(add_tries_data_license(data = query(cars = "Name")))

