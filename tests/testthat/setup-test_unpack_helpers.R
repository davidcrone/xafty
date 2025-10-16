test_unpack_args <- function(quosure, type = "none", link_type = "link", project = "unpack_me", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_args
}

test_create_link <- function(quosure, project = "unpack_me", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  create_link(quosure = quosure, project = project, network = network, ... = ...)
}

test_register <- function(fun, project, network, link_type = "link") {
  quosure <- rlang::enquo(fun)
  register(quosure = quosure, project = project, network = network, link_type = link_type)
}
