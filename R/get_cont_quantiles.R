#' @title Get Continuous Quantiles
#' @description Internal function to get transform continuous quantiles
#' Normalised quantiles (Ordered Quantile Normalisation)
#' and original quantiles
#' @param df a \code{data.frame} with continous variable to be transformed
#' @param varnames Variables (columns) to be transfromed,
#' Default: c("egfr", "hba1c")
#' @param probs a \code(seq) of Probabilities for Quantiles,
#' Default: seq(0, 1, 0.001)
#' @param thin_rdp Whether or not to thin the quantiles, Default: TRUE
#' @param maxpts PARAM_DESCRIPTION, Default: 10
#' @param eps episilon, Default: 0.05
#' @param increment How much to increment epsilon when thinning,
#' Default: 0.01
#' @param maxeps Maximum epsilon when thinning, Default: 1
#' @param ... Additional parameters currently none are supported
#' @return A \code{data.frame} containing the original and transformed quantiles
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'    get_cont_quantiles(ist, varnames = "AGE")
#'  }
#' }
#' @seealso
#'  \code{\link[bestNormalize]{orderNorm}}
#'  \code{\link[RDP]{RamerDouglasPeucker}}
#'  \code{\link[magrittr]{character(0)}}
#'  \code{\link[dplyr]{select}},
#'  \code{\link[dplyr]{reexports}},
#'  \code{\link[dplyr]{distinct}},
#'  \code{\link[dplyr]{bind_rows}}
#'  \code{\link[tidyr]{gather}},
#'  \code{\link[tidyr]{unnest}}
#'  \code{\link[purrr]{map}}
#' @rdname get_continous_quantiles
#' @importFrom bestNormalize orderNorm
#' @importFrom RDP RamerDouglasPeucker
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select ends_with distinct as_tibble bind_rows
#' @importFrom tidyr gather unnest
#' @importFrom purrr map
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

    ## perform transformation and add to dataframe
    res <- bestNormalize::orderNorm(x = df[[varname]])
    df[[paste0(varname, "_t")]] <- res$x.t

    ## get quantiles
    quants <- quants_full <- q <- data.frame(
      probs = probs,
      orig_q  =  quantile(res$x,   probs = probs, na.rm = TRUE),
      tform_q  = quantile(res$x.t, probs = probs, na.rm = TRUE)
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
      approx(
        quants[, "tform_q"],
        quants[, "orig_q"],
        df[[paste0(varname, "_t")]],
        na.rm = FALSE
      )$y
  }

  `%>%` <- magrittr::`%>%`
  df <- df %>%
    dplyr::select(dplyr::ends_with("_q")) %>%
    dplyr::distinct() %>%
    tidyr::gather("varname", "res")
  df$res <- purrr::map(df$res, dplyr::as_tibble)
  df <- dplyr::bind_rows(df) %>%
    tidyr::unnest(res)

  return(df)
}