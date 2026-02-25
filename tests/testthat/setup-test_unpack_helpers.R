test_unpack_args <- function(quosure, type = "none", link_type = "query", project = "unpack_me", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_args
}

test_create_link <- function(quosure, project = "unpack_me", link_type = "query", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  force(project)
  create_link(quosure = quosure, project = project, link_type = link_type, network = network, ... = ...)
}

test_register <- function(fun, project, network, link_type = "query") {
  quosure <- rlang::enquo(fun)
  register(quosure = quosure, project = project, network = network, link_type = link_type)
}

test_build_wrapper_list <- function(network) {
  projects <- names(network)
  li_wrappers <- list()
  for (project in projects) {
    contexts <- names(network[[project]]$ruleset$contexts)
    if(length(contexts) == 0) next
    for (context in contexts) {
      name <- paste0(context, ".", project)
      li_wrappers[[name]] <- list(
        project = project,
        context = context
      )
    }
  }
  li_wrappers
}

context_network_1 <- list(
  projectA = list(ruleset = list(contexts = list())),
  projectB = list(ruleset = list(contexts = list(group = list(on_entry = list(group_by_relation = list()), on_exit  = list(ungroup = list())))))
  )

context_network_2 <- list(
  projectA = list(ruleset = list(contexts = list())),
  projectB = list(ruleset = list(contexts = list(group = list(on_entry = list(group = list()), on_exit = list(ungroup = list()))))),
  projectC = list(ruleset = list(contexts = list(group = list(on_entry = list(group = list()), on_exit = list(ungroup = list()))))),
  projectD = list(ruleset = list(contexts = list(group = list(on_entry = list(group = list()), on_exit = list(ungroup = list())))))
)
