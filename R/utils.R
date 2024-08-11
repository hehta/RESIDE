check_df <- function(
  df
) {
  tryCatch({
    if (!is.data.frame(as.data.frame(df))) {
      return(FALSE)
    }
    return(TRUE)
  }, error = {
    return(FALSE)
  })
}

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