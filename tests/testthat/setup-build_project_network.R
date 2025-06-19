
get_sample_data <- function() {
  data.frame(
    id = 1:5,
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    score = c(85, 92, 78, 90, 88),
    stringsAsFactors = FALSE
  )
}

add_score_category <- function(data) {
  data$category <- ifelse(data$score >= 90, "High", "Low")
  data
}

get_additional_info <- function() {
  data.frame(
    id = c(1, 2, 3, 4, 5),
    department = c("HR", "IT", "Finance", "Marketing", "Sales"),
    stringsAsFactors = FALSE
  )
}

join_datasets <- function(main_data, extra_data) {
  merged <- merge(main_data, extra_data, by = "id", all.x = TRUE)
  merged
}

join_intelligence <- function(main_data, extra_data) {
  merged <- merge(main_data, extra_data, by = "id", all.x = TRUE)
  merged
}

new_column_from_both_projects <- function(data) {
  data$nickname <- paste0(data$department, data$name)
  data
}

intelligence_date <- function() {
  intel <- ceiling(rnorm(5, mean = 100, sd = 10))
  secret_id <-c(1, 2, 3, 4, 5) + 1
  data.frame(
    intelligence = intel,
    secret_id = secret_id
  )
}

mapping_data <- function() {
  secret_id <- c(1, 2, 3, 4, 5) + 1
  data.frame(
    secret_id = secret_id
  )
}

add_decoded_id <- function(data) {
  data$id <-  data$secret_id - 1
  data
}

join_datasets_map <- function(intelligence, map) {
  merged <- merge(intelligence, map, by = "secret_id", all.x = TRUE)
  merged
}

test_network <- init_network()
## Project 1 ##
test_network$new_project("customer_data")
test_network$customer_data$get(get_sample_data())
score_data <- test_network |>  nascent(pull_link(customer_data = c("score", "name")))
test_network$customer_data$add(add_score_category(data = score_data))

## Project 2 ##
test_network$new_project("occupations")
test_network$occupations$get(get_additional_info())

## Join Projects ##
customer_id <- get_sample_data()["id"]
occupation_id <- get_additional_info()["id"]

# Join  projects
test_network$customer_data$join(join_datasets(main_data = pull_link(customer_data = c("id", "category")), extra_data = pull_link(occupations = "id")),
                                with = "occupations")

# Column depending on a two projects
test_network$customer_data$add(new_column_from_both_projects(pull_link(customer_data = "name", occupations = "department")))

link_between_worlds <- pull_link(occupations = "id", customer_data = c("name", "nickname"))
data_worlds_connected <- test_network |> nascent(link_between_worlds)



## Advanced Projects
test_network$new_project("intelligence")
test_network$new_project("map")

test_network$intelligence$get(intelligence_date())
test_network$map$get(mapping_data())
test_network$intelligence$join(with = "map", fun = join_datasets_map(intelligence = pull_link(intelligence = "secret_id"), map = pull_link(map =  "secret_id")))

test_network$map$add(add_decoded_id(pull_link(map = "secret_id")))

test_network$customer_data$join(with = "map", fun = join_intelligence(main_data = pull_link(customer_data = "id"), extra_data = pull_link(map = "id")))

all_joined <- test_network |> nascent(pull_link(occupations = "department", intelligence = "intelligence", customer_data = c("name", "nickname")))


# new_add
add_new_nickname_link <- pull_link(intelligence = "intelligence", customer_data = "nickname")

data <- test_network |> nascent(add_new_nickname_link)

add_new_nickname <- function(data) {
  data$intelligence
  mean_nickname <- ifelse(data$intelligence >= 110, paste0("Smart", data$nickname), paste0("Dumb", data$nickname))
  data$mean_nickname <- mean_nickname
  data
}

test_network$customer_data$add(add_new_nickname(data = add_new_nickname_link))
test_network |> nascent(pull_link(customer_data = "mean_nickname"))
# join between occupations and intelligence

new_pull_link <- pull_link(occupations = "department" ,intelligence = "intelligence")

add_column_to_intelligence <- function(data) {
  data$new_column <- paste0(data$department, data$id)
  data
}

test_network$intelligence$add(add_column_to_intelligence(data = new_pull_link))
test_network |> nascent(pull_link(intelligence = "new_column", customer_data = "mean_nickname"))
