#' @title Import Marginal Distributions
#' @description Import the marginal distribution as exported from a
#' Trusted Research Environment (TRE)
#' @param folder_path Where the marginal distribution files are located,
#' Default: '.' see details.
#' @param binary_variables_file filename for the binary_variables file,
#' Default: '' see details.
#' @param categorical_variables_file filename for the categorical variables file
#' , Default: '' see details.
#' @param continuous_variables_file filename for the continuous variables file,
#' Default: '' see details.
#' @param continuous_quantiles_file filename for the continuous quantiles file,
#' Default: '' see details.
#' @param summary_file filename for the summary file,
#' Default: 'summary.csv' see details.
#' @return Returns an object of a RESIDE class
#' @details This function will import marginal distributions as generated
#' within a Trusted Research Environment (TRE) using the function
#' \code{\link{export_marginal_distributions}}.
#' The folder_path allows the path of the files
#' provided by the TRE to be imported,
#' this will default to the current working directory.
#' The file parameters will provide the default file names
#' if no filenames are specified.
#' @examples
#' \dontrun{
#'   marginals <- import_marginal_distributions()
#' }
#' @seealso
#'  \code{\link{synthesise_data}}
#' @rdname import_marginal_distributions
#' @export
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
import_marginal_distributions <- function(
  folder_path = ".",
  binary_variables_file = "",
  categorical_variables_file = "",
  continuous_variables_file = "",
  continuous_quantiles_file = "",
  summary_file = "summary.csv"
) {
  # Check the folder exists first
  if (! dir.exists(normalizePath(folder_path))) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }

  # Load the binary variables
  .binary_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      binary_variables_file,
      "binary"
    ),
    "binary"
  )

  # Load the categorical variables
  .categorical_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      categorical_variables_file,
      "categorical"
    ),
    "categorical"
  )

  # Load the continuous variables
  .continuous_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      continuous_variables_file,
      "continuous"
    ),
    "continuous"
  )

  # Load the quantiles for the continuous variables
  .quantile_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      continuous_quantiles_file,
      "quantiles"
    ),
    "quantiles"
  )

  .summary_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      summary_file,
      "summary"
    ),
    "summary"
  )

  # Validate the variables and throw an error if they
  # are invalid.
  if (! is_variables_valid(
    .binary_variables,
    .categorical_variables,
    .continuous_variables,
    .quantile_variables,
    .summary_variables
  )) {
    stop("The input files are not valid for the RESIDE package")
  }

  # Forward declare categorical summary list
  .categorical_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(.categorical_variables$variable))) {
    # We required a named vector of int so store this
    n <- .categorical_variables[.categorical_variables$variable == variable, ]$n
    # Set the names of the vector
    names(n) <-
      .categorical_variables[
        .categorical_variables$variable == variable,
      ]$category
    # Add the values to the categorical list
    # Using the variable name as the key
    .categorical_summary[[variable]] <- n
  }

  # Forward declare binary summary list
  .binary_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(.binary_variables$variable))) {
    # Add the mean of the variable to the list
    # Using the variable name as the key
    .binary_summary[[variable]] <- list(
      mean = .binary_variables[.binary_variables$variable == variable, ]$mean,
      missing =
        .binary_variables[.binary_variables$variable == variable, ]$missing
    )
  }

  # Forward declare continuous summary
  .continuous_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(.continuous_variables$variable))) {
    # Get the quantiles as a df for the current variable
    .quantile_df <- as.data.frame(.quantile_variables[
      .quantile_variables$variable == variable,
    ])
    # Set the rownames for equality tests
    rownames(.quantile_df) <- seq_len(nrow(.quantile_df))
    # Set the summary and quantiles for the variable
    # Using the variable name as the key
    .continuous_summary[[variable]] <- list(
      "quantiles" = dplyr::select(
        .quantile_df,
        variable, # nolint: object_name
        orig_q, # nolint: object_name
        tform_q, # nolint: object_name
        epsilon # nolint: object_name
      ),
      "summary" = dplyr::select(
        as.data.frame(.continuous_variables[
          .continuous_variables$variable == variable,
        ],
        row.names = 1L), # Set for equality tests
        mean, # nolint: object_name
        sd, # nolint: object_name
        missing, # nolint: object_name
        max_dp # nolint: object_name
      )
    )
  }

  .summary <- dplyr::select(
    .summary_variables,
    n_row, # nolint: object_name
    n_col, # nolint: object_name
    variables # nolint: object_name
  )

  # Declare Return as a List
  .return <- list(
    categorical_variables = .categorical_summary,
    binary_variables = .binary_summary,
    continuous_variables = .continuous_summary,
    summary = .summary
  )

  # Add a class to the return to allow for S3 overrides
  class(.return) <- "RESIDE"

  # Return the list
  return(.return)

}
