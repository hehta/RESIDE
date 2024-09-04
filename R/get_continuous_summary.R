get_continuous_summary <- function(
  df,
  column_name
) {
  # Store the column as a tempory data frame
  .tmp_df <- df[column_name]
  # Get the quantiles and summaries
  .quantiles_list <- get_cont_quantiles(
    .tmp_df,
    varnames = column_name
  )
  # Extract the quantiles
  .quantiles <- .quantiles_list$quantiles
  # Extract the summary
  .summary <- .quantiles_list$summary

  # Get the number of rows with missing data
  .summary$missing <- get_n_missing(.tmp_df, column_name)
  # Get the maximum number of decimal points
  .summary$max_dp <- max_decimal_places(.tmp_df[[column_name]])
  # Return the quantiles and summary in a list
  return(
    list(
      quantiles = .quantiles,
      summary = .summary
    )
  )
}