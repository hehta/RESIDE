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
  .summary <- .tmp_df %>%
    dplyr::summarise_all(.funs = list(m = mean, s = stats::sd), na.rm = TRUE)
  return(
    list(
      quantiles = .quantiles,
      summary = .summary
    )
  )
}