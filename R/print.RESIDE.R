#' @title print.RESIDE
#' @description S3 override for print RESIDE
#' @param x an object of class RESIDE
#' @param ... Other parameters currently none are used
#' @return No return value, called to print to the terminal.
#' @details S3 Override for RESIDE Class
#' @examples
#' print(
#'   marginal_distributions <- get_marginal_distributions(
#'     IST,
#'     variables <- c(
#'       "SEX",
#'       "AGE",
#'       "ID14",
#'       "RSBP",
#'       "RATRIAL"
#'     )
#'   )
#' )
#' @rdname print.RESIDE
#' @export
#' @importFrom methods is
print.RESIDE <- function(
  x,
  ...
) {
  # Check class
  if (!methods::is(x, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  # If there are any categorical variables
  if ("categorical_variables" %in% names(x)) {
    cat("Summary of Categorical Variables\n")
    # Loop through variables
    for (.variable in names(x[["categorical_variables"]])) {
      # Cat the variable name
      cat(
        "Variable: ",
        .variable,
        "\n"
      )
      # Loop through individual factors
      for (.category in names(x[["categorical_variables"]][[.variable]])) {
        # Ignore Missing @todo deal with missing data
        if (.category == "") next
        # Cat the factor and number of that factor
        cat(
          .category,
          ":",
          x[["categorical_variables"]][[.variable]][[.category]],
          "\n"
        )
      }
    }
  }
  # If there are binary variables
  if ("binary_variables" %in% names(x)) {
    cat("Summary of Binary Variables\n")
    # Loop through the binary variables
    for (.variable in names(x[["binary_variables"]])) {
      cat(
        "Variable: ",
        .variable,
        "\nMean: ",
        x[["binary_variables"]][[.variable]]$mean,
        "\nMissing:",
        x[["binary_variables"]][[.variable]]$missing,
        "\n"
      )
    }
  }
  # If there are any continuous variables
  if ("continuous_variables" %in% names(x)) {
    cat("Summary of Continuous Variables\n")
    # Loop through the continuos variables
    for (.variable in names(x[["continuous_variables"]])) {
      # Cat the variable name
      cat(
        "Variable: ",
        .variable,
        "\n",
        "Quantiles: \n"
      )
      # Print the quantiles without row names
      print(
        data.frame(
          "Original" = x[["continuous_variables"]][[.variable]][["quantiles"]][["orig_q"]], # nolint: line_length
          "Transformed" = x[["continuous_variables"]][[.variable]][["quantiles"]][["tform_q"]], # nolint: line_length
          row.names = NULL
        )
      )
      # Cat the Means and SDs
      cat(
        "Mean: ",
        x[["continuous_variables"]][[.variable]][["summary"]][["mean"]],
        "\nSD: ",
        x[["continuous_variables"]][[.variable]][["summary"]][["sd"]],
        "\nMissing: ",
        x[["continuous_variables"]][[.variable]][["summary"]][["missing"]],
        "\n"
      )
    }
  }
  cat(
    "Summary: \n",
    "Number of Rows: ",
    x[["summary"]][["n_row"]],
    "\nNumber of Columns: ",
    x[["summary"]][["n_col"]],
    "\nVariables: ",
    x[["summary"]][["variables"]]
  )
  invisible()
}