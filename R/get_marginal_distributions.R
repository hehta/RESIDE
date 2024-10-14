#' @title Generate Marginal Distributions for a given data frame
#' @description Generate Marginal Distributions from a given
#' data frame with options to specify which variables to use.
#' @param df Data frame to get the marginal distributions from
#' @param variables (Optional) variable (columns) to select, Default: c()
#' @param print Whether to print the marginal distributions
#' to the console, Default: FALSE
#' @return A list of marginal distributions of an S3 RESIDE Class
#' @details A function to generate marginal distributions from
#' a given data frame, depending on the variable type the marginals
#' will differ, for binary variables a mean and number of missing is generated
#' for continuous variables, they are first transformed and both mean and sd of
#' the transformed variables are stored along with the quantile mapping for back
#' transformation. For categorical variables, the number of each category is
#' stored, missing values are categorise as "missing".
#' @examples
#' marginal_distributions <- get_marginal_distributions(
#'   IST,
#'   variables <- c(
#'     "SEX",
#'     "AGE",
#'     "ID14",
#'     "RSBP",
#'     "RATRIAL"
#'   )
#' )
#' @seealso
#'  \code{\link{export_marginal_distributions}}
#' @rdname get_marginal_distributions
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
get_marginal_distributions <- function(
  df,
  variables = c(),
  print = FALSE
) {
  # Check if variables is set
  df <- as.data.frame(df)
  if (length(variables > 1)) {
    if (!is.character(variables)) {
      stop("Variables must be a vector of characters")
    }
    # Error is any variables are missing
    .missing_variables <- get_missing_variables(df, variables)
    if (length(.missing_variables > 1)) {
      stop(
        paste(
          "all variables must be in df missing:",
          .missing_variables,
          sep = " ",
          collapse = ", "
        )
      )
    }
    # Subset based on variables
    df <- df[variables]
  }

  # Replace missing values for characters with "missing"
  df <- df %>% dplyr::mutate_if(
    is.character,
    function(x) ifelse(x == "", "missing", x)
  )

  # Ensure characters are factors
  df <- df %>% dplyr::mutate_if(is.character, factor)

  # Get variable types
  .variable_types <- get_variable_types(df)
  .categorical_variables <- .variable_types$categorical_variables
  .continuous_variables <- .variable_types$continuous_variables
  .binary_variables <- .variable_types$binary_variables

  # Forward declare binary summary as empty list
  .binary_summary <- list()
  # Loop through binary variables
  for (.column in .binary_variables) {
    # add mean of binary varable to binary summary
    .binary_summary[[.column]] <- list(
      mean = mean(df[[.column]]),
      missing = get_n_missing(df, .column)
    )
  }

  # Forward declare categorical summary as empty list
  .categorical_summary <- list()
  # Loop through categorical variables
  for (.column in .categorical_variables) {
    # add (factor) summary to categorical summary
    .categorical_summary[[.column]] <- summary(df[[.column]])
  }

  # Forward declare continuous summary as empty list
  .continuous_summary <- list()
  # Loop through continuous variables
  for (.column in .continuous_variables) {
    .continuous_summary[[.column]] <- get_continuous_summary(
      df[.column]
    )
  }

  .overall_summary <- data.frame(
    n_row = nrow(df),
    n_col = ncol(df),
    variables = paste(names(df), collapse = ", ")
  )

  # Declare Return as a List
  .return <- list(
    categorical_variables = .categorical_summary,
    binary_variables = .binary_summary,
    continuous_variables = .continuous_summary,
    summary = .overall_summary
  )

  # Add a class to the return to allow for S3 overrides
  class(.return) <- "RESIDE"

  # If print is TRUE print the marginal distributions
  if (print) {
    print(.return)
  }

  # Return the S3 Class
  return(
    .return
  )

}

get_variable_types <- function(df) {
  # Declare variables
  .categorical_variables <- c()
  .continuous_variables <- c()
  .binary_variables <- c()

  # Identify Variable types
  # Loop through Columns
  for (.column in names(df)) {
    # If factor its a categorical variable
    if (is.factor(df[[.column]])) {
      .categorical_variables <- c(
        .categorical_variables,
        .column
      )
      # if numeric it's either binary or continuous
    } else if (is.numeric(df[[.column]])) {
      # If between 0 and 1 it's binary
      if (
        min(df[[.column]], na.rm = TRUE) == 0 &&
          max(df[[.column]], na.rm = TRUE) == 1
      ) {
        .binary_variables <- c(
          .binary_variables,
          .column
        )
        # Otherwise continuous
      } else {
        .continuous_variables <- c(
          .continuous_variables,
          .column
        )
      }
      # If neither factor or numeric, throw an error
    } else {
      stop(
        paste(
          "Unknown Variable type for column",
          .column,
          sep = " "
        )
      )
    }
  }
  return(list(
    categorical_variables = .categorical_variables,
    continuous_variables = .continuous_variables,
    binary_variables = .binary_variables
  ))
}