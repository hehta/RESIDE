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
#'  #EXAMPLE1
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

  data_def <- define_categorical(
    marginals$categorical_variables,
    marginals$summary$n_row,
    data_def
  )

  data_def <- define_binary(
    marginals$binary_variables,
    data_def
  )

  data_def <- define_continuous(
    marginals$continuous_variables,
    data_def
  )

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
  return(sim_df)
}

define_categorical <- function(
  categorical_summary,
  n_row,
  data_def
) {
  .data_def <- data_def
  for (.col in names(categorical_summary)){
    .labs <- c()
    .probs <- c()
    for (.cat in names(categorical_summary[[.col]])) {
      .labs <- c(.labs, .cat)
      .probs <- c(.probs, (categorical_summary[[.col]][[.cat]] / n_row))
    }
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "categorical",
      formula = paste0(.probs, collapse = ";"),
      variance = paste0(.labs, collapse = ";")
    )
  }
  return(.data_def)
}

define_binary <- function(
  binary_summary,
  data_def
) {
  .data_def <- data_def
  for (.col in names(binary_summary)){
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "binary",
      formula = binary_summary[[.col]]
    )
  }
  return(.data_def)
}

define_continuous <- function(
  continuous_summary,
  data_def
) {
  .data_def <- data_def
  for (.col in names(continuous_summary)) {
    .data_def <- simstudy::defData(
      .data_def,
      varname = .col,
      dist = "normal",
      formula = continuous_summary[[.col]][["summary"]][["m"]],
      variance = continuous_summary[[.col]][["summary"]][["s"]]
    )
  }
  return(.data_def)
}

add_missingness <- function(
  simulated_data,
  marginals
) {
  # Todo
}