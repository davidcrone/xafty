data_sm <- function() {

  state_env <- new.env()

  set_data_key <- function(project, key) {
    state_env$projects[[project]]$key <- key
  }

  get_data_key <- function(project) {
    state_env$projects[[project]]$key
  }

  set_data <- function(data, key) {
    state_env$data[[key]] <- data
  }

  get_data <- function(project) {
    key <- get_data_key(project)
    state_env$data[[key]]
  }

  get_data_by_key <- function(key) {
    state_env$data[[key]]
  }

  get_projects_by_key <- function(key) {
    projects <- names(state_env$projects)
    contains_key <- vapply(projects, \(project) {
      key_value <- state_env$projects[[project]]$key
      if(is.null(key_value)) return(FALSE)
      key_value == key
    } , FUN.VALUE = logical(1))
    names(contains_key)[contains_key]
  }

  set_object <- function(object_key, data) {
    state_env$objects[[object_key]] <- data
  }

  get_object <- function(object_key) {
    state_env$objects[[object_key]]
  }

  set_states <- function(states) {
    state_env$states <- states
  }

  get_state <- function(name) {
    if(is_curly_character(name)) {
      name <- get_braced_variable(name)
    }
    state_env$states[[name]]
  }

  list(
    set_data_key = set_data_key,
    get_data_key = get_data_key,
    set_data = set_data,
    get_data = get_data,
    get_projects_by_key = get_projects_by_key,
    get_data_by_key = get_data_by_key,
    set_object = set_object,
    get_object = get_object,
    set_states = set_states,
    get_state = get_state,
    get_projects = get_projects
  )
}

build_tree <- function(network) {
  tree_env <- new.env()
  tree_env$query <- query()
  tree_env$objects <- list()
  tree_env$joins <- list()
  tree_env$network$settings <- network$settings
  tree_env$network$states <- network$states

  # Nodes of the directed (hopefully) acyclic graph
  set_nodes <- function(link, code) {
    node <- code$node
    if(!is.null(node)) {
      node_name <- names(node)
      tree_env$codes[[node_name]] <- node[[node_name]]
      tree_env$links[[node_name]] <- link
    }
    ## set joins
    joins <- code$joins
    if(length(joins) != 0)
    for (join in names(joins)) {
      set_join(id = join, projects = joins[[join]])
    }
  }

  append_deps <- function(name, deps) {
    tree_env$codes[[name]] <- deps
  }

  get_codes <- function() {
    tree_env$codes
  }

  get_links <- function() {
    tree_env$links
  }

  set_object <- function(object_code, dag) {
    tree_env$objects[[object_code]] <- dag
  }

  get_object_codes <- function() {
    names(tree_env$objects)
  }

  get_objects <- function() {
    tree_env$objects
  }

  set_query <- function(query) {
    tree_env$query <- merge_queries(tree_env$query, query)
  }

  get_query <- function() {
    tree_env$query
  }

  set_join <- function(id, projects) {
    tree_env$joins[[id]] <- projects
  }

  get_joins <- function() {
    tree_env$joins
  }

  set_join_path <- function(path) {
    tree_env$join_path <- path
  }

  get_join_path <- function() {
    if(is.null(tree_env$join_path)) return(list())
    tree_env$join_path
  }

  set_mask <- function(mask) {
    current_masks <- tree_env$masks
    for (i in seq_along(mask)) {
      col <- names(mask[i])
      projects <- mask[[i]]
      current_masks[[col]] <- unique(c(current_masks[[col]], projects))
    }
    tree_env$masks <- current_masks
  }

  get_mask <- function() {
    tree_env$masks
  }

  get_network_state <- function() {
    tree_env$network
  }

  set_wrapper_projects <- function(projects) {
    tree_env$wrappers <- unique(c(tree_env$wrappers, projects))
  }

  get_wrapper_projects <- function() {
    tree_env$wrappers
  }

  list(
    set_nodes = set_nodes,
    append_deps = append_deps,
    get_codes = get_codes,
    get_links = get_links,
    get_query = get_query,
    set_query = set_query,
    set_object = set_object,
    get_objects = get_objects,
    set_join = set_join,
    get_joins = get_joins,
    get_object_codes = get_object_codes,
    set_join_path = set_join_path,
    get_join_path = get_join_path,
    set_mask = set_mask,
    get_mask = get_mask,
    get_network_state = get_network_state,
    set_wrapper_projects = set_wrapper_projects,
    get_wrapper_projects = get_wrapper_projects
  )
}
