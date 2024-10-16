#' @title Synthesise data from marginal distributions
#' @description Allows the synthesis of data from marginal
#' distributions obtained from a Trusted Research Environment (TRE)
#' @param marginals an object of class RESIDE
#' @param correlation_matrix Correlation Matrix
#' see \code{\link{export_empty_cor_matrix}} and
#' \code{\link{import_cor_matrix}}, Default: NULL
#' @param ... Additional parameters currently none are used.
#' @return a data frame of simulated data
#' @details This function will synthesise a dataset from marginals imported
#' using \code{\link{import_marginal_distributions}}.
#' By default the dataset will not contain correlations,
#' however user specified correlations can be added using
#' the \code{correlation_matrix} parameter,
#' see \code{\link{export_empty_cor_matrix}} and
#' \code{\link{import_cor_matrix}} for more details.
#' @examples
#' \dontrun{
#'    marginals <- import_marginal_distributions()
#'    df <- synthesise_data(marginals)
#' }
#' @seealso
#'  \code{\link{export_empty_cor_matrix}}
#'  \code{\link{import_cor_matrix}}
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
  # @todo add covariance matrix for correlations
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
  # Predefine dataDefinition
  data_def <- get_data_def(marginals, FALSE)

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

# Internal function to synthesise data with correlations
synthesise_data_cor <- function(
  marginals,
  correlation_matrix
) {
  data_def <- get_data_def(marginals, TRUE)

  # Synthesise the data
  sim_df <- simstudy::genCorFlex(
    marginals$summary$n_row,
    data_def,
    corMatrix = correlation_matrix
  )

  # Replace dummy categories where rows add up to more than 1
  sim_df <- fix_factors(
    sim_df,
    marginals
  )

  sim_df <- restore_factors(
    sim_df,
    marginals$categorical_variables
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
  # Reorder dataframe
  column_names <- c("id", get_data_def(marginals)[["varname"]])
  sim_df <- sim_df %>%
    dplyr::select(dplyr::any_of(column_names))
  # Return the data frame
  return(sim_df)
}

get_data_def <- function(
  marginals,
  use_correlations = FALSE
) {
  # Predefine dataDefinition
  data_def <- NULL

  # If there are categorical variables
  if ("categorical_variables" %in% names(marginals)) {
    # Define categorical variables dependant on correlations
    if (use_correlations) {
      data_def <- define_categorical_binary(
        marginals$categorical_variables,
        marginals$summary$n_row,
        data_def
      )
    } else {
      data_def <- define_categorical(
        marginals$categorical_variables,
        marginals$summary$n_row,
        data_def
      )
    }
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
  return(data_def)
}

define_categorical <- function(
  categorical_summary,
  n_row,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(categorical_summary)){
    # Forward declare the categories
    .labs <- c()
    # Forward declare the probabilities
    .probs <- c()
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Add the category to the categories
      .labs <- c(.labs, .cat)
      # Add the probability to the probabilities
      .probs <- c(.probs, (categorical_summary[[.column]][[.cat]] / n_row))
    }
    # Add the definition to the definitions
    # Specifying a categorical distribution
    # Giving the categories and probabilities of each
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
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
  data_def = NULL
) {
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(categorical_summary)){
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Add the category to the definitions as dummy (binary) variables
      .data_def <- simstudy::defData(
        .data_def,
        varname = paste(.column, .cat, sep = "_"),
        dist = "binary",
        formula = (categorical_summary[[.column]][[.cat]] / n_row)
      )
    }
  }
  # Return the definitions
  return(.data_def)
}

define_binary <- function(
  binary_summary,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(binary_summary)){
    # Add the variable to the definitions
    # Specifying a binary distribution
    # Using mean as the probability
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
      dist = "binary",
      formula = binary_summary[[.column]][["mean"]]
    )
  }
  return(.data_def)
}

define_continuous <- function(
  continuous_summary,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(continuous_summary)) {
    # Add the variable to the definition
    # Specifying a normal distribution
    # Using mean and standard deviation
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
      dist = "normal",
      formula = continuous_summary[[.column]][["summary"]][["mean"]],
      variance = continuous_summary[[.column]][["summary"]][["sd"]]
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
#' the required variables as a csv file.
#' @param marginals The marginal distributions
#' @param folder_path Folder to export to.
#' @param file_name (optional) file name, Default: 'correlation_matrix.csv'
#' @param create_folder Whether the folder should be created, Default: TRUE
#' @return No return value, called for exportation of files.
#' @details This function will export an empty correlation matrix
#' as a csv file, it will contain all the necessary variables including
#' dummy variables for factors. Dummy variables for factors may contain
#' a missing category to represent missing data. Correlations should be
#' added to the empty CSV and the imported using the
#' \code{\link{import_marginal_distributions}} function.
#' Correlations should be supplied using rank order correlations.
#' The correlation matrix should be symmetric and positive semi definite.
#' @examples
#' \dontrun{
#'  marginals <- import_marginal_distributions()
#'  export_empty_cor_matrix(
#'    marginals,
#'    folder_path = tempdir()
#'   )
#' }
#' @seealso
#'  \code{\link{import_marginal_distributions}}
#'  \code{\link{import_cor_matrix}}
#' @rdname export_empty_cor_matrix
#' @export
#' @importFrom simstudy genCorMat
#' @importFrom utils write.csv
export_empty_cor_matrix <- function(
  marginals,
  folder_path,
  file_name = "correlation_matrix.csv",
  create_folder = TRUE
) {
  # Check folder path
  if (missing(folder_path)){
    stop("A folder path must be provided.")
  }
  # Check class
  if (!methods::is(marginals, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  if (create_folder) {
    # Create the folder, ignore warnings (folder exists)
    dir.create(folder_path, showWarnings = FALSE)
  }
  # Check the folder exists (even if created)
  if (! dir.exists(folder_path)) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }
  # Get the data definition, to get all the variable names,
  # including the dummy variables
  data_def <- get_data_def(marginals, TRUE)
  # Generate an empty correlation matrix with a default correlation of 0
  .cor_matrix <- simstudy::genCorMat(nrow(data_def), rho = 0)
  # Set the column names
  colnames(.cor_matrix) <- data_def[["varname"]]
  # Set the row names
  rownames(.cor_matrix) <- data_def[["varname"]]
  # Join the file path
  .file_path <- file.path(normalizePath(folder_path), file_name)
  # Write the CSV with the row names
  utils::write.csv(.cor_matrix, .file_path, row.names = TRUE)
  invisible(NULL)
}

#' @title Import a correlation matrix
#' @description Imports a correlation matrix from a csv file generated by
#' \code{\link{export_empty_cor_matrix}}
#' @param file_path A path to the csv file, Default: './correlation_matrix.csv'
#' @return a matrix of correlations that can be used with
#' \code{\link{synthesise_data}}
#' @details A function to import the user specified correlations
#' generated from the csv file exported by the
#' \code{\link{export_empty_cor_matrix}} function.
#' Correlations should be entered into the CSV file,
#' using rank order correlations. The correlation matrix
#' should be symmetric and be positive semi definite.
#' @examples
#' \dontrun{
#'   import_cor_matrix("correlation_matrix.csv")
#' }
#' @seealso
#'  \code{\link{export_empty_cor_matrix}}
#'  \code{\link[matrixcalc]{is.positive.semi.definite}}
#' @rdname import_cor_matrix
#' @export
#' @importFrom utils read.csv
#' @importFrom tibble column_to_rownames
#' @importFrom matrixcalc is.positive.semi.definite
import_cor_matrix <- function(
  file_path = "./correlation_matrix.csv"
) {
  if (!file.exists(file_path)) {
    stop("Correlation file must exist.")
  }
  .cor_matrix <- utils::read.csv(file_path)
  .cor_matrix <- .cor_matrix %>%
    tibble::column_to_rownames(names(.cor_matrix)[1])
  .cor_matrix <- as.matrix(.cor_matrix)
  if (!isSymmetric(.cor_matrix)) {
    stop("The correlation matrix needs to be symmetrical.")
  }
  if (!matrixcalc::is.positive.semi.definite(.cor_matrix)) {
    stop("The correlation matrix needs to be positive semi definite.")
  }
  return(.cor_matrix)
}

# With correlations it is possible that the dummy variables for a single
# category do not add up to one, we will fix that here
fix_factors <- function(
  simulated_data,
  marginals
) {
  # Get the categorical summary from the marginals
  categorical_summary <- marginals$categorical_variables
  # Extract the number for rows from the marginals
  n_row <- marginals$summary$n_row
  # Loop through the columns
  for (.column in names(categorical_summary)){
    # Forward declare category names
    cat_names <- c()
    # Forward declare probabilities
    probs <- c()
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Get the dummy variable name for the category
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the dummy variable name to the category names
      cat_names <- c(cat_names, cat_name)
      # Calculate the probability for the category
      # and add it to the probabilities
      probs <- c(probs, (categorical_summary[[.column]][[.cat]] / n_row))
    }
    # Subset only the dummy variables for the current category
    variable_df <- simulated_data[, cat_names]

    # Check that all the probabilities are unique
    probs <- check_probs(probs)

    # Replace any row where the dummy variables total 0
    # (indicating no category was selected)
    variable_df[rowSums(variable_df) == 0, ] <-
      replace_zero_rows(variable_df[rowSums(variable_df) == 0, ], probs)

    # Replace any row where the dummy variables total 1
    # (indicating more than one category was selected)
    variable_df[rowSums(variable_df) > 1, ] <-
      replace_one_rows(variable_df[rowSums(variable_df) > 1, ], probs)
    # Replace the columns with the corrected columns
    simulated_data[, cat_names] <- variable_df
  }
  # Return the corrected data frame
  return(simulated_data)
}

# Function to replace a row for dummy categorical
# variables where the dummy columns add up to zero
# indicating that a category has not been selected
# in which case the category with the highest probability
# is selected
replace_zero_rows <- function(rows, probs) {
  rtn_row <- as.data.frame(rbind(probs))
  rtn_row[rtn_row != max(probs)] <- 0
  rtn_row[rtn_row == max(probs)] <- 1
  return(rtn_row)
}

# Function to replace a row for dummy categorical
# variables where the dummy columns add up more than one
# indicating that more than one category has been selected
# in which case out of the categories selected,
# the one with the higher probability is chosen.
replace_one_rows <- function(rows, probs) {
  # Turn probabilities into row in data frame
  prob_rows <- as.data.frame(rbind(probs))
  # duplicate the row to match the number of rows in `rows`
  prob_rows <- rbind(prob_rows, prob_rows[rep(1, nrow(rows) - 1), ])
  # multiply the rows by the probabilities
  rtn_rows <- as.data.frame(as.matrix(rows) * as.matrix(prob_rows))
  # add the maximum value for each row
  rtn_rows$row_max <- apply(rtn_rows, 1, max, na.rm = TRUE)
  # take the maximum value to a new data frame
  rtn_rows_max <- rtn_rows[, "row_max"]
  # remove the maximum value from the old data frame
  rtn_rows$row_max <- NULL
  # divide each column in the rows by the maximum values
  # leaving 1 for the column with the max value
  rtn_rows <- rtn_rows / rtn_rows_max
  # set all the other columns to 0
  rtn_rows[rtn_rows != 1] <- 0
  return(rtn_rows)
}

# Convert factors back from dummy categories
restore_factors <- function(
  simulated_data,
  categorical_summary
) {
  # Forward declare the category names
  category_names <- c()
  # loop through the categorical variables
  for (.column in names(categorical_summary)){
    # Add the variable as a column to the data frame
    simulated_data[[.column]] <- ""
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Define the dummy category name
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the category to the list of category names
      category_names <- c(category_names, cat_name)
      # If the indicator (1) for the dummy variable set the category
      # to the current category
      simulated_data[simulated_data[[cat_name]] == 1, ][[.column]] <- .cat
    }
  }
  # Remove dummy variables
  simulated_data[, category_names] <- list(NULL)
  # Return the modified data
  return(simulated_data)
}

# There is a small chance that the probabilities could be equal
# if this is the case add noise to allow them to be used to select the
# highest probability.
check_probs <- function(probs) {
  while (any(duplicated(probs))) {
    probs <- jitter(probs)
  }
  return(probs)
}