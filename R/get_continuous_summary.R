get_continuous_summary <- function(
  df,
  column_name
) {
  `%>%` <- magrittr::`%>%`
  .tmp_df <- df[column_name]
  .quantiles <- get_cont_quantiles(
    .tmp_df,
    varnames = column_name
  )
  # Get the summary statistics (mean / SD)
  .summary <- .tmp_df %>%
    dplyr::summarise_all(.funs = list(m = mean, s = stats::sd), na.rm = TRUE)
  # Get the number of rows with missing data
  .summary$missing <- nrow( # Number of Rows with just NA
    as.data.frame( # Ensure it's a df as subsetting a single column
      .tmp_df[is.na(.tmp_df[column_name]), ]
    )
  )
  return(
    list(
      quantiles = .quantiles,
      summary = .summary
    )
  )
}