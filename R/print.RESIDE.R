#' @title print.RESIDE
#' @description S3 override for print RESIDE
#' @param x an object of class RESIDE
#' @param ... Other parameters currently none are used
#' @return NULL
#' @details S3 Override for RESIDE Class
#' @examples
#' \dontrun{
#' if(interactive()){
#'    print(
#'      marginal_distributions <- get_marginal_distributions(
#'        IST,
#'        variables <- c(
#'          "SEX",
#'          "AGE",
#'          "ID14",
#'          "RSBP",
#'          "RATRIAL"
#'        )
#'      )
#'    )
#'  }
#' }
#' @rdname print.RESIDE
#' @export
#' @importFrom methods is
print.RESIDE <- function(
  x,
  ...
) {
  if (!methods::is(x, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  if ("categorical_variables" %in% names(x)) {
    cat("Summary of Categorical Variables\n")
    for (.variable in names(x[["categorical_variables"]])) {
      cat(
        "Variable: ",
        .variable,
        "\n"
      )
      for (.category in names(x[["categorical_variables"]][[.variable]])) {
        if (.category == "") next
        cat(
          .category,
          ":",
          x[["categorical_variables"]][[.variable]][[.category]],
          "\n"
        )
      }
    }
  }
  if ("binary_variables" %in% names(x)) {
    cat("Summary of Binary Variables\n")
    for (.variable in names(x[["binary_variables"]])) {
      cat(
        "Variable: ",
        .variable,
        "\n",
        "Mean: ",
        x[["binary_variables"]][[.variable]],
        "\n"
      )
    }
  }
  if ("continuous_variables" %in% names(x)) {
    cat("Summary of Continuous Variables\n")
    for (.variable in names(x[["continuous_variables"]])) {
      cat(
        "Variable: ",
        .variable,
        "\n",
        "Quantiles: \n"
      )
      print(
        data.frame(
          "Original" = x[["continuous_variables"]][[.variable]][["quantiles"]][["orig_q"]],
          "Transformed" = x[["continuous_variables"]][[.variable]][["quantiles"]][["tform_q"]],
          row.names = NULL
        )
      )
      cat(
        "Mean: ",
        x[["continuous_variables"]][[.variable]][["summary"]][["m"]],
        "\n",
        "SD: ",
        x[["continuous_variables"]][[.variable]][["summary"]][["s"]],
        "\n"
      )
}
  }
}