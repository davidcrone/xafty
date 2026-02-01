
to_script <- function(dag, file = NULL) {
  #stopifnot(!is.null(file))
  order_of_execution <- dag$execution_order

  blocks <- new.env()
  li_write <- list()
  for (func_name in order_of_execution) {
    li_write[[func_name]] <- write_function_call(func_name = func_name, dag = dag, blocks = blocks)
  }
  write_vec <- unlist(li_write)
  writeLines(write_vec, con = file, sep = "\n")
}

write_function_call <- function(func_name, dag, blocks) {
  link <- dag$links[[func_name]]
  func <- link$fun
  formals <- write_formals_definition(func = func)
  definition <- paste0(func_name, " <- function(", formals, ") {")
  body <- deparse(body(func))[-1]
  execution <- write_function_execution(func_name = func_name, link = link, blocks = blocks)
  set_block(link = link, blocks = blocks)
  func_block <- c(definition, body, "", execution, "")
  func_block
}

write_formals_definition <- function(func) {
  fmls <- formals(func)
  if (is.null(fmls)) return(character())

  vec_formals <- vapply(seq_along(fmls), \(i) {
      name <- names(fmls)[i]
      default <- fmls[[i]]
      if (missing(default)) {
        name
      } else {
        paste0(name, " = ", paste(deparse(default), collapse = " "))
      }
    },
    character(1)
  )
  paste0(vec_formals, collapse = ", ")
}

build_args_line <- function(args, blocks) {
  if(length(args) == 0) return(NULL)
  xafty_types <- vapply(args, find_xafty_objects, FUN.VALUE = character(1))
  vec_execution <- lapply(seq_along(xafty_types), \(i) {
    xafty_type <- xafty_types[i]
    arg <- args[[i]]
    if(xafty_type == "xafty_query") {
      lead_proj <- get_lead_project(arg)
      name_block <- blocks[[lead_proj]]
      paste0(names(xafty_type), " = ", name_block)
    } else {
      paste0(names(xafty_type), " = ", arg)
    }
  })
  paste0(vec_execution, collapse = ", ")
}

set_block <- function(link, blocks) {
  type <- link$type
  project <- link$project
  if(type == "get") {
    blocks[[project]] <- project
  } else if(type == "join") {
    xafty_objects <- vapply(link$args, find_xafty_objects, FUN.VALUE = character(1))
    xafty_query <- names(xafty_objects)[xafty_objects == "xafty_query"]
    projects <- vapply(xafty_query, \(name) get_lead_project(link$args[[name]]), FUN.VALUE = character(1), USE.NAMES = FALSE)

    # Name of the data.frame variable when joined
    project_code <- paste0(projects, collapse = ".")

    # Additional projects need to be also renamed when the join was already performed earlier
    additional_projects <- get_project_by_code(projects = projects, blocks = blocks)
    projects <- c(projects, additional_projects)
    for (project in projects) {
      blocks[[project]] <- project_code
    }
  } else {
    xafty_objects <- vapply(link$args, find_xafty_objects, FUN.VALUE = character(1))
    xafty_query <- names(xafty_objects)[xafty_objects == "xafty_query"]
    lead_project <- get_lead_project(link$args[[xafty_query]])
    code <- blocks[[project]]
    if(is.null(code)) {
      blocks[[project]] <- blocks[[lead_project]]
    } else {
      blocks[[project]] <- code
    }
  }
  blocks
}

write_function_execution <- function(func_name, link, blocks) {
  project <- link$project
  formals_ex <- build_args_line(args = link$args, blocks = blocks)
  if(link$type == "get") {
    paste0(project, " <- ", func_name, "(", formals_ex, ")")
  } else if(link$type == "join") {
    code <- build_project_code(link)
    paste0(code, " <- ", func_name, "(", formals_ex, ")")
  } else {
    code <- blocks[[project]]
    if(is.null(code)) code <- build_project_code(link = link)
    paste0(code, " <- ", func_name, "(", formals_ex, ")")
  }

}

build_project_code <- function(link) {
  xafty_objects <- vapply(link$args, find_xafty_objects, FUN.VALUE = character(1))
  xafty_query <- names(xafty_objects)[xafty_objects == "xafty_query"]
  projects <- vapply(xafty_query, \(name) get_lead_project(link$args[[name]]), FUN.VALUE = character(1), USE.NAMES = FALSE)
  project_code <- paste0(projects, collapse = ".")
}

get_codes <- function(projects, blocks) {
  codes <- lapply(projects, \(project) blocks[[project]])
  unlist(codes)
}

get_project_by_code <- function(projects, blocks) {
  codes <- get_codes(projects = projects, blocks = blocks)
  all_projects <- names(blocks)
  possible <- all_projects[!all_projects %in% projects]
  if(length(possible) == 0) return(character(0))
  projects_tangled <- lapply(possible, \(project) if (blocks[[project]] %in% codes) return(project) else character(0))
  unlist(projects_tangled)
}
