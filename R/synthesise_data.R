#' @title Synthesise data from marginal distributions
#' @description Allows the synthesis of data from marginal
#' distributions obtained from a Trusted Research Environment (TRE)
#' @param marginals an object of class RESIDE
#' @param covariance_matrix Covariance Matrix not yet implemented, Default: NULL
#' @param ... Additional parameters currently none are used.
#' @return a data frame of simulated data
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'    marginals <- get_marginal_distributions(IST)
#'    df <- synthesise_data(marginals)
#'  }
#' }
#' @seealso
#'  \code{\link[methods]{is}}
#' @rdname synthesise_data
#' @export
#' @importFrom methods is
#' @importFrom simstudy defData
#' @importFrom simstudy genData
synthesise_data <- function(
  marginals,
  covariance_matrix = NULL,
  ...
) {
  # Check class
  if (!methods::is(marginals, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  # @todo add covaraince matrix for correlations
  if (!is.null(covariance_matrix)) {
    stop("correllations a not yet supported")
  } else {
    synthesise_data_no_cor(marginals)
  }
}

#' @rdname synthesise_data
#' @export
synthesize_data <- synthesise_data

# Internal function to synthesise data without correlations
synthesise_data_no_cor <- function(
  marginals
) {
  # Predifine dataDefinition
  data_def <- NULL

  # If there are categorical variables
  if ("categorical_variables" %in% names(marginals)) {
    # Define categorical variables
    data_def <- define_categorical(
      marginals$categorical_variables,
      marginals$summary$n_row,
      data_def
    )
  }

  # If there are binary variables
  if ("binary_variables" %in% names(marginals)) {
    # Define binary variables
    data_def <- define_binary(
      marginals$binary_variables,
      data_def
    )
  }

  # If there are continuous variables
  if ("continuous_variables" %in% names(marginals)) {
    # Define continuous variables
    data_def <- define_continuous(
      marginals$continuous_variables,
      data_def
    )
  }

  # Synthesise the data
  sim_df <- simstudy::genData(
    marginals$summary$n_row,
    data_def
  )

  # Back transform continuous variables
  for (variable_name in names(marginals$continuous_variables)) {
    sim_df[[variable_name]] <-
      approx(
        marginals$continuous_variables[[variable_name]]$quantiles$tform_q,
        marginals$continuous_variables[[variable_name]]$quantiles$orig_q,
        xout = sim_df[[variable_name]], rule = 2:1
      )$y
    # Round to original decimal places
    sim_df[[variable_name]] <- round(
      sim_df[[variable_name]],
      marginals$continuous_variables[[variable_name]]$summary$max_dp
    )
  }
  # Add missing values (MAR)
  sim_df <- add_missingness(
    sim_df,
    marginals
  )
  # Return the data frame
  return(sim_df)
}

define_categorical <- function(
  categorical_summary,
  n_row,
  data_def
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.col in names(categorical_summary)){
    # Forward declare the categories
    .labs <- c()
    # Forward declare the probabilities
    .probs <- c()
    # Loop through the categories
    for (.cat in names(categorical_summary[[.col]])) {
      # Add the category to the categories
      .labs <- c(.labs, .cat)
      # Add the probability to the probabilities
      .probs <- c(.probs, (categorical_summary[[.col]][[.cat]] / n_row))
    }
    # Add the definition to the definitions
    # Specifying a categorical distribution
    # Giving the categories and probabilities of each
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "categorical",
      formula = paste0(.probs, collapse = ";"),
      variance = paste0(.labs, collapse = ";")
    )
  }
  # Return the definitions
  return(.data_def)
}

define_binary <- function(
  binary_summary,
  data_def
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.col in names(binary_summary)){
    # Add the variable to the definitions
    # Specifying a binary distribution
    # Using mean as the probability
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "binary",
      formula = binary_summary[[.col]][["mean"]]
    )
  }
  return(.data_def)
}

define_continuous <- function(
  continuous_summary,
  data_def
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.col in names(continuous_summary)) {
    # Add the variable to the definition
    # Specifying a normal distribution
    # Using mean and standard deviation
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "normal",
      formula = continuous_summary[[.col]][["summary"]][["mean"]],
      variance = continuous_summary[[.col]][["summary"]][["sd"]]
    )
  }
  return(.data_def)
}

add_missingness <- function(
  simulated_data,
  marginals
) {
  # Remove purposly added 'missing' factors
  .df <- simulated_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character), ~gsub("missing", "", .x)
      )
    )
  # Loop through the binary variables
  for (binary_variable in names(marginals$binary_variables)) {
    # Get the number of NAs
    .variable_missingness <-
      marginals$binary_variables[[binary_variable]][["missing"]]
    # Only add NAs if there are any to add
    if (.variable_missingness > 0) {
      # Select a random set of rows given the number of NAs and replace
      # the variable of those rows with NAs
      .df[sample(nrow(.df), .variable_missingness), ][[binary_variable]] <- NA
    }
  }
  # Loop through the continuous variables
  for (continuous_variable in names(marginals$continuous_variables)) {
    # Only add NAs if there are any to add
    .variable_missingness <-
      marginals$continuous_variables[[continuous_variable]][["summary"]][["missing"]] # nolint line_length
    if (.variable_missingness > 0) {
      # Select a random set of rows given the number of NAs and replace
      # the variable of those rows with NAs
      .df[sample(nrow(.df), .variable_missingness), ][[continuous_variable]] <- NA # nolint line_length
    }
  }
  # Return the new data frame
  return(.df)
}