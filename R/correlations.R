#'
#' @title Create a correlation object
#' @description A helper function to create a correlation object
#' @param x The name of the first variable
#' @param y The name of the second variable
#' @param rho The correlation between the two variables
#' @param ... Additional arguments to specify data frame names and factor names
#' See details for more information on the additional arguments.
#' @return A list containing the correlation information
#' @details This function is a helper function to create a correlation
#' object that can be used to specify correlations between variables
#' when synthesising data using the \code{\link{synthesise_data}} function.
#' Additional Arguments:
#' \itemize{
#' \item df_name: The name of the data frame containing both variables
#' \item df_name.x: The name of the data frame containing the first variable
#' \item df_name.y: The name of the data frame containing the second variable
#' \item factor_name.x: The name of the factor variable for the first variable
#' \item factor_name.y: The name of the factor variable for the second variable
#' }
#' @examples
#'  correlation("age", "bmi", 0.5)
#' @rdname correlation
#' @export
correlation <- function(
  x,
  y,
  rho,
  ...
) {
  if (!is.numeric(rho) || rho < -1 || rho > 1) {
    stop("Correlation coefficient (rho) must be a numeric value between -1 and 1.")
  }
  if (!is.character(x) || !is.character(y)) {
    stop("Variable names (x and y) must be character strings.")
  }
  varargs <- list(...)
  correlation <- list(
    x = x,
    y = y,
    rho = rho
  )
  if ("df_name" %in% names(varargs)) {
    correlation$df_name <- varargs$df_name
  }
  if ("df_name.x" %in% names(varargs)) {
    if (! "df_name.y" %in% names(varargs)) {
      stop("Both df_name.x and df_name.y must be provided if one is specified.")
    }
    correlation$df_name.x <- varargs$df_name.x
  }
  if ("df_name.y" %in% names(varargs)) {
    if (! "df_name.x" %in% names(varargs)) {
      stop("Both df_name.x and df_name.y must be provided if one is specified.")
    }
    correlation$df_name.y <- varargs$df_name.y
  }
  if ("factor_name.x" %in% names(varargs)) {
    correlation$factor_name.x <- varargs$factor_name.x
  }
  if ("factor_name.y" %in% names(varargs)) {
    correlation$factor_name.y <- varargs$factor_name.y
  }
  class(correlation) <- "correlation"
  correlation
}

get_correlation_names <- function(
  correlations
) {
  correlation_names <- c()
  for (cor in correlations) {
    correlation_names <- c(correlation_names, cor$x, cor$y)
  }
  return(unique(correlation_names))
}
