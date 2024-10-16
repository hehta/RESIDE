#' @title Export Marginal Distributions
#' @description Export the marginal distributions to CSV files
#' @param marginals an Object of type RESIDE from
#' \code{\link{import_cor_matrix}}
#' @param folder_path path to folder where to save files.
#' @param create_folder if the folder does not exist should it be created,
#' Default: FALSE
#' @param force if the folder already contains marginal distribution files
#' should they be removed, Default: FALSE
#' @return No return value, called for exportation of files.
#' @details Exports each of the marginal distributions to CSV files
#' within a given folder, along with the continuous quantiles.
#' @examples
#' \donttest{
#'   marginal_distributions <- get_marginal_distributions(IST)
#'   export_marginal_distributions(
#'     marginal_distributions,
#'     folder_path = tempdir()
#'   )
#' }
#' @seealso
#'  \code{\link{get_marginal_distributions}}
#' @rdname export_marginal_distributions
#' @export
#' @importFrom methods is
export_marginal_distributions <- function(
  marginals,
  folder_path,
  create_folder = FALSE,
  force = FALSE
) {
  # Check folder path
  if (missing(folder_path)){
    stop("A folder path must be provided.")
  }
  # Check class
  if (!methods::is(marginals, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  # if the user wants the folder to be created
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
  # Check existing files
  # Get a list of existing files
  .existing_files <- marginal_files_exist(folder_path)
  # Check if list is empty
  if (length(.existing_files) > 0) {
    # if not forcing through an error
    if (!force) {
      stop(paste(
        "Marginal files:",
        .existing_files,
        "already exists",
        sep = " ",
        collapse = ", "
      ))
    }
    # Otherwise (try to) remove the files
  } else {
    remove_marginal_files(folder_path)
  }
  # Check there are categorical variables
  if ("categorical_variables" %in% names(marginals)) {
    # Generate the absolute os appropriate file path
    .file_path <- get_full_file_path(
      folder_path,
      "categorical_variables.csv"
    )
    # Convert the marginals to a data frame
    .categorical_df <- categorical_to_df(marginals$categorical_variables)
    # Write the file if there are any rows
    if (nrow(.categorical_df) > 0) {
      .write_csv(.categorical_df, .file_path, "categorical")
    }
  }

  # Check there are categorical variables
  if ("binary_variables" %in% names(marginals)) {
    # Generate the absolute os appropriate file path
    .file_path <- get_full_file_path(
      folder_path,
      "binary_variables.csv"
    )
    .binary_df <- binary_to_df(marginals$binary_variables)
    # Write the file if there are any rows
    if (nrow(.binary_df) > 0) {
      .write_csv(.binary_df, .file_path, "binary")
    }
  }

  if ("continuous_variables" %in% names(marginals)) {
    # Generate the absolute os appropriate file path for marginals
    .file_path <- get_full_file_path(
      folder_path,
      "continuous_variables.csv"
    )
    # Convert the marginals to a data frame
    .continuous_df <- continuous_to_df(marginals$continuous_variables)
    # Write the file if there are any rows
    if (nrow(.continuous_df) > 0) {
      .write_csv(.continuous_df, .file_path, "continuous")
    }
    # Convert the quantiles to a data frame
    .quantiles_df <- quantiles_to_df(marginals$continuous_variables)
    # Generate the absolute os appropriate file path for quantiles
    .file_path <- get_full_file_path(
      folder_path,
      "continuous_quantiles.csv"
    )
    # Write the file if there are any rows
    if (nrow(.quantiles_df) > 0) {
      .write_csv(.quantiles_df, .file_path, "quantiles")
    }
  }
  # Write the summary file (needed for the number of rows)
  .write_csv(
    marginals[["summary"]],
    get_full_file_path(
      folder_path,
      "summary.csv"
    ),
    "summary"
  )
  invisible(NULL)
}