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

  get_object_names <- function() {
    names(state_env$objects)
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
    get_object_names = get_object_names
  )
}

build_tree <- function() {
  tree_env <- new.env()
  tree_env$query <- query()
  tree_env$objects <- query()

  # Nodes of the directed (hopefully) acyclic graph
  set_nodes <- function(link, code) {
    node_name <- names(code)
    tree_env$codes[[node_name]] <- code[[node_name]]
    tree_env$links[[node_name]] <- link
  }

  get_codes <- function() {
    tree_env$codes
  }

  get_links <- function() {
    tree_env$links
  }

  set_objects <- function(object_query) {
    for (oq in object_query) {
      object_key <- paste0(oq[[1]]$from, ".", get_squared_variable(oq[[1]]$select))
      tree_env$objects[[object_key]] <- oq
    }
  }

  get_object_names <- function() {
    names(tree_env$objects)
  }

  get_object_query <- function(object_key) {
    tree_env$objects[[object_key]]
  }

  set_query <- function(query) {
    tree_env$query <- merge_queries(tree_env$query, query)
  }

  get_query <- function() {
    tree_env$query
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

  set_join_pairs <- function(li_pairs) {
    join_code <- names(li_pairs)
    tree_env$join_pairs[[join_code]] <- li_pairs[[join_code]]
  }

  get_join_pairs <- function() {
    tree_env$join_pairs
  }

  list(
    set_nodes = set_nodes,
    get_codes = get_codes,
    get_links = get_links,
    get_query = get_query,
    set_query = set_query,
    set_objects = set_objects,
    get_object_names = get_object_names,
    get_object_query = get_object_query,
    set_join_path = set_join_path,
    get_join_path = get_join_path,
    set_join_pairs = set_join_pairs,
    get_join_pairs = get_join_pairs,
    set_mask = set_mask,
    get_mask = get_mask
  )
}
