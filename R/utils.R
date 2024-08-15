get_missing_variables <- function(
  df,
  variables
) {
  # Set up a vector for missing variables
  .missing_variables <- c()
  for (variable in variables) {
    if (!variable %in% names(df)) {
      .missing_variables <- c(.missing_variables, variable)
    }
  }
  return(.missing_variables)
}

binary_to_df <- function(
  x
) {
  `%>%` <- magrittr::`%>%`
  binary_df <- as.data.frame(x$binary_variables)
  binary_df <- binary_df %>%
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "variable",
      values_to = "mean"
    )
  return(binary_df)
}

quantiles_to_df <- function(
  x
) {
  .quantiles <- lapply(x, \(.x) .x[["quantiles"]])
  .quantiles <- do.call(rbind, .quantiles)
  return(.quantiles)
}