pull_link <- function(...) {
  list_pulls <- list(...)
  list_xafty_links <- lapply(names(list_pulls), \(project) {
    xafty_link <- list(pull = list_pulls[[project]],
                       from = project)
    class(xafty_link) <- c("list", "xafty_pull")
    xafty_link
  })
  names(list_xafty_links) <- names(list_pulls)
  class(list_xafty_links) <- c("list", "xafty_link_list")
  list_xafty_links
}
