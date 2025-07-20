test_unpack_args <- function(quosure, type = "none", module = "link", project = "unpack_me", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_args
}

test_arg_dependencies <- function(quosure, type = "none", module = "link", project = "unpack_me", network = test_network, ...) {
  quosure <- rlang::enquo(quosure)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  links <- vapply(list_args$args, \(arg) find_xafty_objects(arg), FUN.VALUE = character(1))
  arg_defs <- build_arg_defs(links)
  li_arg_dependencies <- gather_dependenucies_per_arg(args = list_args$args, defs = arg_defs, network = network)
  li_arg_dependencies
}

test_create_link <- function(quosure, project = "unpack_me", network = test_network) {
  quosure <- rlang::enquo(quosure)
  create_link(quosure = quosure, project = project, network = network)
}
