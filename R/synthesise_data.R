#' @title Synthesise data from marginal distributions
#' @description Allows the synthesis of data from marginal
#' distributions obtained from a Trusted Research Environment (TRE)
#' @param marginals an object of class RESIDE
#' @param correlation_matrix Correlation Matrix
#' see \code{\link{export_empty_cor_matrix}}, Default: NULL
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
  correlation_matrix = NULL,
  ...
) {
  # Check class
  if (!methods::is(marginals, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  # @todo add covaraince matrix for correlations
  if (!is.null(correlation_matrix)) {
    synthesise_data_cor(marginals, correlation_matrix)
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

# Internal function to synthesise data without correlations
synthesise_data_cor <- function(
  marginals,
  correlation_matrix
) {
  # Predifine dataDefinition
  data_def <- NULL

  # If there are categorical variables
  if ("categorical_variables" %in% names(marginals)) {
    #Define categorical variables
    data_def <- define_categorical_binary(
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
  sim_df <- simstudy::genCorFlex(
    marginals$summary$n_row,
    data_def,
    corMatrix = correlation_matrix
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

define_categorical_binary <- function(
  categorical_summary,
  n_row,
  data_def
) {
  .data_def <- data_def
  # Loop through the variables
  for (.col in names(categorical_summary)){
    for (.cat in names(categorical_summary[[.col]])) {
      .data_def <- simstudy::defData(
        .data_def,
        varname = paste0(.col, "_", .cat),
        dist = "binary",
        formula = (categorical_summary[[.col]][[.cat]] / n_row)
      )
    }
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

#' @title Export an empty correlation matrix
#' @description A function to export a correlation matrix with
#' the required variables as a csv file
#' @param marginals The marginal distributions
#' @param folder_path Folder to export to, Default: '.'
#' @param create_folder Whether the folder should be created, Default: TRUE
#' @return NULL
#' @details This function will export an empty correlation matrix
#' as a csv file, it will contain all the necessary variables including
#' dummy variables for factors.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  marginals <- get_marginal_distributions(IST)
#'  export_empty_cor_matrix
#'  }
#' }
#' @seealso
#'  \code{\link[simstudy]{genCorMat}}
#'  \code{\link[rio]{export}}
#' @rdname export_empty_cor_matrix
#' @export
#' @importFrom simstudy genCorMat
#' @importFrom rio export
export_empty_cor_matrix <- function(
  marginals,
  folder_path = ".",
  create_folder = TRUE
) {
  if (create_folder) {
    # Create the folder, ignore warnings (folder exists)
    dir.create(folder_path, showWarnings = FALSE)
  }
  # Check the foleder exists (even if created)
  if (! dir.exists(folder_path)) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }
  .df <- synthesise_data_cor(marginals, NULL)
  .cor_matrix <- simstudy::genCorMat(ncol(.df) - 1, rho = 0)
  colnames(.cor_matrix) <- names(.df)[2:length(names(.df))]
  rownames(.cor_matrix) <- names(.df)[2:length(names(.df))]
  rio::export(.cor_matrix, "correlation_matrix.csv", row.names = TRUE)
  invisible(NULL)
}

#' @title Import a correlation matrix
#' @description Imports a correlation matrix from a csv file generated by
#' \code{\link{export_empty_cor_matrix}}
#' @param file_path A path to the csv file
#' @return a matrix of correlations that can be used with
#' \code{\link{synthesise_data}}
#' @details A function to import the user specified correlations
#' generated from the csv file exported by the
#' \code{\link{export_empty_cor_matrix}} function
#' @examples
#' \dontrun{
#' if(interactive()){
#'    import_cor_matix("correlation_matrix.csv")
#'  }
#' }
#' @seealso
#'  \code{\link[rio]{import}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[matrixcalc]{is.positive.semi.definite}}
#' @rdname import_cor_matrix
#' @export
#' @importFrom rio import
#' @importFrom tibble column_to_rownames
#' @importFrom matrixcalc is.positive.semi.definite
import_cor_matrix <- function(
  file_path
) {
  if (!file.exists(normalizePath(file_path))) {
    stop("Correlation file must exist")
  }
  .cor_matrix <- rio::import(file_path)
  .cor_matrix <- .cor_matrix %>%
    tibble::column_to_rownames(names(.cor_matrix)[1])
  .cor_matrix <- as.matrix(.cor_matrix)
  if (!isSymmetric(.cor_matrix)) {
    stop("The correlation matrix needs to be symetrical")
  }
  if (!matrixcalc::is.positive.semi.definite(.cor_matrix)) {
    stop("The correlation matrix needs to be positive semi definite")
  }
  return(.cor_matrix)
}