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
