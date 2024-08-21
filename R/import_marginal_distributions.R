import_marginal_distributions <- function(
  folder_path = ".",
  binary_variables_file = "",
  categorical_variables_file = "",
  continuous_variables_file = "",
  continuous_quantiles_file = ""
) {
  # Check the folder exists first
  if (! dir.exists(normalizePath(folder_path))) {
    stop(
      "Directory must exist, hint: set create_folder to TRUE"
    )
  }
  .binary_variables_file <- get_variables_path(
    folder_path,
    binary_variables_file,
    "binary"
  )
  .categorical_variables_file <- get_variables_path(
    folder_path,
    categorical_variables_file,
    "categorical"
  )
  .continuous_variables_file <- get_variables_path(
    folder_path,
    continuous_variables_file,
    "continuous"
  )
  .continuous_quantiles_file <- get_variables_path(
    folder_path,
    continuous_quantiles_file,
    "quantiles"
  )
}
