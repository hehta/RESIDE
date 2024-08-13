#' @title Generate Marginal Distributions for a given data frame
#' @description Generate Marginal Distibutions from a given
#' data frame with options to specify which variables to use.
#' @param df PARAM_DESCRIPTION
#' @param variables PARAM_DESCRIPTION, Default: c()
#' @param ignore_na PARAM_DESCRIPTION, Default: TRUE
#' @param print PARAM_DESCRIPTION, Default: TRUE
#' @return A lisd of and S3 RESIDE Class
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'    marginal_distributions <- get_marginal_distributions(
#'      IST,
#'      variables <- c(
#'        "SEX",
#'        "AGE",
#'        "ID14",
#'        "RSBP",
#'        "RATRIAL"
#'      )
#'    )
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate_all}}
#' @rdname get_marginal_distributions
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate_if
get_marginal_distributions <- function(
  df,
  variables = c(),
  ignore_na = TRUE,
  print = TRUE
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

  # Scope Magnittr pipe
  `%>%` <- magrittr::`%>%`

  # Ensure characters are factors
  df <- df %>% dplyr::mutate_if(is.character, factor)

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
      # if numberic it's either binary or continuous
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
      # If neither factor or numeric, through and error
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

  # Forward declare binary summary as empty list
  .binary_summary <- list()
  # Loop through binary variables
  for (.column in .binary_variables) {
    # add mean of binary varable to binary summary
    .binary_summary[[.column]] <- mean(df[[.column]])
  }

  # Forward declare categorical summary as empty list
  .categorical_summary <- list()
  # Loop through categorical variables
  for (.column in .categorical_variables) {
    # add (factor) summary to categortical summary
    .categorical_summary[[.column]] <- summary(df[[.column]])
  }

  # Forward declare continuous summary as empty list
  .continuous_summary <- list()
  # Loop through continuous variables
  for (.column in .continuous_variables) {
    .continuous_summary[[.column]] <- get_continuous_summary(
      df,
      .column
    )
  }

  # Declare Return as a List
  .return <- list(
    categorical_variables = .categorical_summary,
    binary_variables = .binary_summary,
    continuous_variables = .continuous_summary
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