pass_through <- function(data) {
  data
}


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
  intel <- c(120, 99, 100, 130, 80)
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

test_network <- init_network("test_network")
## Project 1 ##
test_network$add_project("customer_data")
test_network$customer_data$link(get_sample_data())
test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))))
## Project 2 ##
test_network$add_project("occupations")
test_network$occupations$link(get_additional_info())

# Join  projects
test_network$customer_data$link(join_datasets(main_data = query(customer_data = c("id", "category")),
                                              extra_data = query(occupations = "id")), vars = character(0), direction = "both")
# Column depending on a two projects
test_network$customer_data$link(new_column_from_both_projects(query(customer_data = "name", occupations = "department")))

## Advanced Projects
test_network$add_project("intelligence")
test_network$add_project("map")

test_network$intelligence$link(intelligence_date())
test_network$map$link(mapping_data())
test_network$map$link(fun = join_datasets_map(intelligence = query(intelligence = "secret_id"), map = query(map =  "secret_id")), direction = "both")

test_network$map$link(add_decoded_id(query(map = "secret_id")))

test_network$customer_data$link(fun = join_intelligence(main_data = query(customer_data = "id"), extra_data = query(map = "id")), direction = "both")

# new_add
add_new_nickname <- function(data) {
  data$intelligence
  mean_nickname <- ifelse(data$intelligence >= 110, paste0("Smart", data$nickname), paste0("Dumb", data$nickname))
  data$mean_nickname <- mean_nickname
  data
}

test_network$customer_data$link(add_new_nickname(data = query(customer_data = "nickname", intelligence = "intelligence")))
# join between occupations and intelligence
add_column_to_intelligence <- function(data) {
  data$new_column <- paste0(data$department, data$id)
  data
}

test_network$intelligence$link(add_column_to_intelligence(data = query(map = "id", occupations = "department")))

