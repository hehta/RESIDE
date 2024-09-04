get_continuous_summary <- function(
  df,
  column_name
) {
  .tmp_df <- df[column_name]
  .quantiles_list <- get_cont_quantiles(
    .tmp_df,
    varnames = column_name
  )
  .quantiles <- .quantiles_list$quantiles
  .summary <- .quantiles_list$summary

  # Get the number of rows with missing data
  .summary$missing <- nrow( # Number of Rows with just NA
    as.data.frame( # Ensure it's a df as subsetting a single column
      .tmp_df[is.na(.tmp_df[column_name]), ]
    )
  )
  .summary$max_dp <- max_decimal_places(.tmp_df[[column_name]])
  return(
    list(
      quantiles = .quantiles,
      summary = .summary
    )
  )
}