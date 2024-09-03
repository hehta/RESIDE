get_cont_quantiles <- function(
  df,
  varnames = c("egfr", "hba1c"),
  probs =  seq(0, 1, 0.001),
  thin_rdp = TRUE,
  maxpts = 10,
  eps = 0.05,
  increment = 0.01,
  maxeps = 1,
  ...
) {
  if (any(paste0(varnames, "_t") %in% names(df))) {
    stop("Transformations already present")
  }

  for (varname in varnames) {
    if (!varname %in% names(df)) {
      stop("Varname needs for be in data frame")
    }

    ## perform transformation and add to dataframe
    res <- suppressWarnings(
      bestNormalize::orderNorm(x = df[[varname]])
    )
    df[[paste0(varname, "_t")]] <- res$x.t

    ## get quantiles
    quants <- quants_full <- q <- data.frame(
      probs = probs,
      orig_q  =  stats::quantile(res$x,   probs = probs, na.rm = TRUE),
      tform_q  = stats::quantile(res$x.t, probs = probs, na.rm = TRUE)
    )

    ## Apply RDP function to reduce the number of points
    if (thin_rdp) {
      pts <- nrow(q)
      mye <- eps
      while (pts > maxpts) {
        q <- RDP::RamerDouglasPeucker(
          quants_full$orig_q,
          quants_full$tform_q,
          epsilon = mye
        )
        q <- as.data.frame(q)
        names(q) <- c("orig_q", "tform_q")
        q$epsilon <- mye
        pts <- nrow(q)
        mye <- mye + increment
        if (mye >= maxeps) break
      }
      quants <- q
    }

    ## store quantiles in tibble (note same for every row)
    df[[paste0(varname, "_q")]] <- vector(mode = "list", length = nrow(df))
    df[[paste0(varname, "_q")]] <-
      lapply(
        df[[paste0(varname, "_q")]],
        function(x) quants
      )

    ## recover original vector from quantiles and transformed vector
    df[[paste0(varname, "_r")]] <-
      stats::approx(
        quants[, "tform_q"],
        quants[, "orig_q"],
        df[[paste0(varname, "_t")]],
        na.rm = FALSE
      )$y
  }

  df <- df %>%
    dplyr::select(dplyr::ends_with("_q")) %>%
    dplyr::distinct() %>%
    tidyr::gather("varname", "res")
  df$res <- purrr::map(df$res, dplyr::as_tibble)
  df <- dplyr::bind_rows(df) %>%
    tidyr::unnest(res)

  return(df)
}