#' @title print.RESIDE
#' @description S3 override for print RESIDE
#' @param x an object of class RESIDE
#' @param ... Other parameters, \code{full = TRUE} prints every category
#' of the categorical variables, Default: FALSE
#' @return The RESIDE object, invisibly. Called to print to the terminal.
#' @details S3 Override for RESIDE Class, prints the overall summary
#' followed by the marginal distributions of each data frame.
#' By default categorical variables with more than 10 categories only
#' print the 10 most common categories, use \code{full = TRUE} to print
#' every category.
#' @examples
#' print(
#'   marginal_distributions <- get_marginal_distributions(
#'     IST,
#'     variables = c(
#'       "SEX",
#'       "AGE",
#'       "ID14",
#'       "RSBP",
#'       "RATRIAL"
#'     )
#'   )
#' )
#' print(marginal_distributions, full = TRUE)
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
  varargs <- list(...)
  # Print every category if full is specified
  full <- isTRUE(varargs[["full"]])
  single_table <- is_single_table(x)
  # Print the overall summary
  overall_summary <- x[["overall_summary"]]
  cat("Summary of Marginal Distributions\n")
  if (!single_table) {
    cat(
      "Number of Data Frames:",
      overall_summary[["n_data_frames"]],
      "\nData Frames:",
      overall_summary[["data_frame_names"]],
      "\n"
    )
  }
  if (overall_summary[["subject_identifier"]] != "") {
    cat(
      "Subject Identifier:",
      overall_summary[["subject_identifier"]],
      "\n"
    )
  }
  cat(
    "Number of Subjects:",
    overall_summary[["n_subjects"]],
    "\n"
  )
  if (!single_table) {
    cat(
      "Common Columns:",
      overall_summary[["common_columns"]],
      "\n"
    )
  }
  # Print the marginals of each data frame
  for (df_name in get_df_names_or_key(x)) {
    if (!single_table) {
      cat("\nData Frame:", df_name, "\n")
    }
    print_sub_marginals(x[[df_name]], full)
  }
  invisible(x)
}

# Internal function to print the marginals of a single data frame
print_sub_marginals <- function(
  sub_marginals,
  full = FALSE,
  max_categories = 10
) {
  cat(
    "\nNumber of Rows:",
    sub_marginals[["summary"]][["n_row"]],
    "\nNumber of Columns:",
    sub_marginals[["summary"]][["n_col"]],
    "\nVariables:",
    sub_marginals[["summary"]][["variables"]],
    "\n"
  )
  # If there are any categorical variables
  categorical_variables <- sub_marginals[["categorical_variables"]]
  if (length(categorical_variables) > 0) {
    cat("\nSummary of Categorical Variables\n")
    # Loop through variables
    for (.variable in names(categorical_variables)) {
      cat("Variable:", .variable, "\n")
      .categories <- categorical_variables[[.variable]]
      n_hidden <- 0
      # Unless full, only print the most common categories
      if (!full && length(.categories) > max_categories) {
        n_hidden <- length(.categories) - max_categories
        .categories <- sort(.categories, decreasing = TRUE)[
          seq_len(max_categories)
        ]
      }
      # Loop through individual categories, including missing
      for (.category in names(.categories)) {
        cat(
          " ",
          .category,
          ":",
          .categories[[.category]],
          "\n"
        )
      }
      if (n_hidden > 0) {
        cat(
          "  ... and",
          n_hidden,
          "more categories, use print(x, full = TRUE) to print all\n"
        )
      }
    }
  }
  # If there are any binary variables
  binary_variables <- sub_marginals[["binary_variables"]]
  if (length(binary_variables) > 0) {
    cat("\nSummary of Binary Variables\n")
    # Loop through the binary variables
    for (.variable in names(binary_variables)) {
      cat(
        "Variable:",
        .variable,
        "\n  Mean:",
        binary_variables[[.variable]][["mean"]],
        "\n  Missing:",
        binary_variables[[.variable]][["missing"]],
        "\n"
      )
    }
  }
  # If there are any continuous variables
  continuous_variables <- sub_marginals[["continuous_variables"]]
  if (length(continuous_variables) > 0) {
    cat("\nSummary of Continuous Variables\n")
    # Loop through the continuous variables
    for (.variable in names(continuous_variables)) {
      .summary <- continuous_variables[[.variable]][["summary"]]
      .quantiles <- continuous_variables[[.variable]][["quantiles"]]
      is_date <- isTRUE(.summary[["is_date"]])
      original <- .quantiles[["orig_q"]]
      # Dates are stored as days since the epoch
      if (is_date) {
        original <- as.Date(original, origin = "1970-01-01")
      }
      cat(
        "Variable:",
        .variable,
        "\n  Date:",
        is_date,
        "\n  Missing:",
        .summary[["missing"]],
        "\n  Decimal Places:",
        .summary[["max_dp"]],
        "\n  Quantiles:\n"
      )
      # Print the quantiles without row names
      print(
        data.frame(
          "Original" = original,
          "Transformed" = .quantiles[["tform_q"]],
          row.names = NULL
        )
      )
    }
  }
}
