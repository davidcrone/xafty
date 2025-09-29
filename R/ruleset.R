ruleset <- function() {
  list(
    "modules" = list(
      "link" = list(
      ),
      "object" = list(
      )
    )
  )
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

add_to_ruleset <- function(item, module = "link", env, project, ...) {
  function_name <- item$fun_name
  .dots <- list()

  # When registering an object, the object should only be registered in the project
  if(!"object_name" %in% names(.dots)) {
    projects <- unique(c(project, get_lead_projects(item)))
  }
  new_rule <- list(item)
  new_rule <- setNames(new_rule, function_name)
  for (proj in projects) {
    current_rules <- env[[proj]]$ruleset$modules[[module]]
    if(function_name %in% names(current_rules)) {
      if(!exists("user_input")) {
        user_input <- readline(paste0("Function '", function_name, "' was already registered in project '",  paste0(projects, collapse = " and "),"'. Would you like to update? (y/n): "))
      }
      # Check the user input
      if (user_input == "y") {
        # Proceed with the update
        current_rules[[function_name]] <- NULL
        # Add your update code here
      } else {
        # Abort the function
        stop(paste0("Function '", item$fun_name, "' exists already in ruleset of project '", paste0(projects, collapse = " and "),"'"))
      }
    }
    add_rules <- c(current_rules, new_rule)
    env[[proj]]$ruleset$modules[[module]] <- add_rules
  }
  env
}
