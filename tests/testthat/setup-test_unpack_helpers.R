test_unpack <- init_network()
test_unpack$add_project("unpack_me")


test_unpack_args <- function(quosure, type = "none", module = "link", project = "unpack_me", network = test_unpack, ...) {
  quosure <- rlang::enquo(quosure)
  unpacked <- unpack(quosure = quosure, network = network, project = project)
  fun_exp <- rlang::get_expr(quosure)
  fun_env <- rlang::get_env(quosure)
  list_args <- unpack_args(exp = fun_exp, env = fun_env)
  list_args
}
