# Internal function to generate continuous marginals
# and corresponding quantile mapping.
get_continuous_summary <- function(
  column,
  probs =  seq(0, 1, 0.001),
  maxpts = 10,
  eps = 0.05,
  increment = 0.01,
  maxeps = 1,
  ...
) {
  tryCatch({
    variable <- names(column)
    quantile_df <- na.omit(column)
    ## perform transformation and add to dataframe
    res <- suppressWarnings(
      bestNormalize::orderNorm(x = quantile_df[[variable]])
    )
    quantile_df[[paste0(variable, "_t")]] <- res$x.t

    ## get quantiles
    quants <- quants_full <- q <- data.frame(
      probs = probs,
      orig_q  =  stats::quantile(res$x,   probs = probs, na.rm = TRUE),
      tform_q  = stats::quantile(res$x.t, probs = probs, na.rm = TRUE)
    )
    # Set initial number points
    pts <- nrow(q)
    # copy epsilon
    mye <- eps
    # Loop until max points
    while (pts > maxpts) {
      # Use RDP to thin and store in q
      q <- RDP::RamerDouglasPeucker(
        quants_full$orig_q,
        quants_full$tform_q,
        epsilon = mye
      )
      # Convert q back to data frame
      q <- as.data.frame(q)
      # Add back names
      names(q) <- c("orig_q", "tform_q")
      # Set the current epsilon
      q$epsilon <- mye
      # Update the number of points
      pts <- nrow(q)
      # Increase the epison value
      mye <- mye + increment
      # Escape is maximum epsilon has been reached
      if (mye >= maxeps) break
    }
    # Update quants to thinned values
    quants <- q
    # Add the variable name to the quantiles
    quants$variable <- variable
    # Reorder the quantiles
    quants <- quants %>% dplyr::select(
      variable,
      orig_q,
      tform_q,
      epsilon
    )
    # Summarise the transformed quantiles
    summary_df <- quantile_df[paste0(variable, "_t")] %>%
      dplyr::summarise_all(
        .funs = list(
          mean = mean, sd = sd
        ),
        na.rm = TRUE
      )

    # Get the number of rows with missing data
    summary_df$missing <- get_n_missing(column, variable)
    # Get the maximum number of decimal points
    summary_df$max_dp <- max_decimal_places(column[[variable]])

    # Return both the quantiles and summaries of the quantiles
    return(list(
      quantiles = quants,
      summary = summary_df
    ))
  }, error = function(e) {
    warning(
      paste0(
        "Could not transform variable ",
        variable,
        "\nError Message: ",
        e$message,
        "\nskipping"
      )
    )
    return(NULL)
  })
}
