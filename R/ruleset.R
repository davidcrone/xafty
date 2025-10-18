ruleset <- function(link_types = c("link", "object")) {
  sapply(link_types, \(link_type) list())
}

settings <- function(network_name) {
  if(length(network_name) == 0 | length(network_name) > 1) stop("Please enter the network's name.")
  if(!identical(network_name, make.names(network_name))) stop("Please enter a valid network name")
    list(
      network_name = network_name,
      state = list(
        global_default = NULL
      )
    )
}

add_to_ruleset <- function(item, link_type = "link", network, project, ...) {
  function_name <- item$fun_name
  .dots <- list(...)
  # When registering an object, the object should only be registered in the project
  if(is.null(.dots[["object_name"]])) {
    projects <- unique(c(project, get_lead_projects(item)))
  } else {
    projects <- project
  }
  new_rule <- list(item)
  new_rule <- setNames(new_rule, function_name)
  for (proj in projects) {
    current_rules <- network[[proj]]$ruleset[[link_type]]
    if(function_name %in% names(current_rules)) {
      update <- isTRUE(.dots[["update"]])
      if(!update) {
        user_update <- readline(paste0("Function '", function_name, "' was already registered in project '",  paste0(projects, collapse = " and "),"'. Would you like to update? (y/n): "))
        if(user_update == "y") {
          update <- TRUE
        } else {
          update <- FALSE
        }
      }
      # Check the user input
      if (update) {
        # Proceed with the update
        # remove the previously registered variables
        clean_variables <- current_rules[[function_name]]$variables
        rm(list = clean_variables, envir = network[[project]]$variables)
        current_rules[[function_name]] <- NULL
        if(exists("user_update")) {
          message(paste0("Updated function '", function_name, "'!"))
        }
        # Add your update code here
      } else {
        # Abort the function
        message(paste0("Function '", item$fun_name, "' exists already in ruleset of project '", paste0(projects, collapse = " and "),"'"))
      }
    }
    add_rules <- c(current_rules, new_rule)
    network[[proj]]$ruleset[[link_type]] <- add_rules
  }
  network
}
