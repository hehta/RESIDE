#' @title Export Marginal Distributions
#' @description Export the marginal distributions to CSV files
#' @param x an Object of type RESIDE from get_marginal_distributions()
#' @param folder_path path to folder where to save files, Default: '.'
#' @param create_folder if the folder does not exist should it be created,
#' Default: FALSE
#' @return No Explicit Return
#' @details Exports each of the marginal distributions to CSV files
#' within a given folder, along with the continuous quantiles.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    marginal_distributions <- get_marginal_distributions(IST)
#'    export_marginal_distributions(marginal_distributions)
#'  }
#' }
#' @seealso
#'  \code{\link[methods]{is}}
#'  \code{\link[utils]{write.table}}
#' @rdname export_marginal_distributions
#' @export
#' @importFrom methods is
#' @importFrom utils write.csv
export_marginal_distributions <- function(
  x,
  folder_path = ".",
  create_folder = FALSE
) {
  # Check class
  if (!methods::is(x, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  # if the user wants the folder to be created
  if (create_folder) {
    # Create the folder, ignore warnings (folder exists)
    dir.create(normalizePath(folder_path), showWarnings = FALSE)
  }
  # Check the foleder exists (even if created)
  if (! dir.exists(normalizePath(folder_path))) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }
  # Check there are categorical variables
  if ("categorical_variables" %in% names(x)) {
    # Generate the absolute os appropriate file path
    .file_path <- generate_file_path(
      "categorical_variables.csv",
      folder_path,
      "Categorical Variables"
    )
    # Convert the marginals to a data frame
    .categorical_df <- categorical_to_df(x$categorical_variables)
    # Write the file
    utils::write.csv(.categorical_df, .file_path)
  }

  # Check there are categorical variables
  if ("binary_variables" %in% names(x)) {
    # Generate the absolute os appropriate file path
    .file_path <- generate_file_path(
      "binary_variables.csv",
      folder_path,
      "Binary Variables"
    )
    .binary_df <- binary_to_df(x$binary_variables)
    # Write the file
    utils::write.csv(.binary_df, .file_path)
  }

  if ("continuous_variables" %in% names(x)) {
    # Generate the absolute os appropriate file path for marginals
    .file_path <- generate_file_path(
      "continuous_variables.csv",
      folder_path,
      "Continuous Variables"
    )
    # Convert the marginals to a data frame
    .continuous_df <- continuous_to_df(x$continuous_variables)
    # Write the file for marginals
    utils::write.csv(.continuous_df, .file_path)
    # Convert the quantiles to a data frame
    .quantiles_df <- quantiles_to_df(x$continuous_variables)
    # Generate the absolute os appropriate file path for quantiles
    .file_path <- generate_file_path(
      "continuous_quantiles.csv",
      folder_path,
      "Continuous Quantiles"
    )
    # Write the file for quantiles
    utils::write.csv(.quantiles_df, .file_path)
  }
  # Write the summary file (needed for the number of rows)
  utils::write.csv(
    x[["summary"]],
    generate_file_path(
      "summary.csv",
      folder_path,
      "Summary"
    )
  )

}