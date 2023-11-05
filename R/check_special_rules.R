# TODO: The values in a column with the unique rule should only have one representation each; like a primary key!
check_columns_unique <- function(check_table, validity_table) {
  xafty_syntax <- "##!!unique"

  columns <- obtain_columns_in_validity(validity_table, xafty_syntax = xafty_syntax)
  n_columns <- length(columns)

  for (col in seq(n_columns)) {




  }
}
