export_marginal_distributions <- function(
  x,
  folder_path = ".",
  create_folder = FALSE
) {
  if (!methods::is(x, "RESIDE")) {
    stop("object must be of class RESIDE")
  }

  if (create_folder) {
    dir.create(normalizePath(folder_path), showWarnings = FALSE)
  }

  if (! dir.exists(normalizePath(folder_path))) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }

  if ("categorical_variables" %in% names(x)) {
    .file_path <- generate_file_path(
      "categorical_variables.csv",
      folder_path,
      "Categorical Variables"
    )
    .categorical_df <- categorical_to_df(x$categorical_variables)
    utils::write.csv(.categorical_df, .file_path)
  }

  if ("binary_variables" %in% names(x)) {
    .file_path <- generate_file_path(
      "binary_variables.csv",
      folder_path,
      "Binary Variables"
    )
    .binary_df <- binary_to_df(x$binary_variables)
    utils::write.csv(.binary_df, .file_path)
  }

  if ("continuous_variables" %in% names(x)) {
    .file_path <- generate_file_path(
      "continuous_variables.csv",
      folder_path,
      "Continuous Variables"
    )
    .continuous_df <- continuous_to_df(x$continuous_variables)
    utils::write.csv(.continuous_df, .file_path)
    .quantiles_df <- quantiles_to_df(x$continuous_variables)
    .file_path <- generate_file_path(
      "continuous_quantiles.csv",
      folder_path,
      "Continuous Quantiles"
    )
    utils::write.csv(.quantiles_df, .file_path)
  }

}