ruleset <- function() {
  list(
    "modules" = list(
      "link" = list(
      )
      # Modules for future expansion
    )
  )
}

settings <- function() {
    list(
      "meta_data_name" = "meta_data",
      "path_project_env" = NULL
    )
}

add_to_ruleset <- function(item, module = "link", env, project) {
  function_name <- item$fun_name
  projects <- unique(c(project, get_lead_projects(item)))
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
        stop(paste0("Rule ", names(item), " already exists in ruleset of project '", paste0(projects, collapse = " and "),"'. Cannot register a rule that already exists."))
      }
    }
    add_rules <- c(current_rules, new_rule)
    env[[proj]]$ruleset$modules[[module]] <- add_rules
  }
  env
}
