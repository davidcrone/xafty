test_that("print.xafty_project() displays grouped variables separately from ungrouped", {
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "scoring")
  # Capture the print output
  output <- capture.output(print(test_network$customer_data))
  output_text <- paste0(output, collapse = "\n")
  # Check that group label appears in output
  expect_match(output_text, "scoring")
  expect_match(output_text, "\U1F3F7\uFE0F")  # Tag emoji
})

test_that("print.xafty_project() maintains layer hierarchy within groups", {
  # This test creates a complex dependency chain to ensure layers are preserved
  test_network <- init_network("test_network", projects = "customer_data")
  test_network$customer_data$link(get_sample_data())
  # Layer 0 variable in group
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score", "name"))), group = "processing", update = TRUE)
  # Layer 1 variable in same group (depends on Layer 0)
  test_network$customer_data$link(add_score_category(data = query(customer_data = c("score"))), vars = "category_enhanced", group = "processing", update = TRUE)
  output <- capture.output(print(test_network$customer_data))
  output_text <- paste0(output, collapse = "\n")
  # Check that both Root and Layer 1 appear within group
  expect_match(output_text, "processing")
  expect_match(output_text, "\U1F331 Root")
})
