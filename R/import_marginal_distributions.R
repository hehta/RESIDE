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

  .binary_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      binary_variables_file,
      "binary"
    ),
    "binary"
  )

  .categorical_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      categorical_variables_file,
      "categorical"
    ),
    "categorical"
  )

  .continuous_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      continuous_variables_file,
      "continuous"
    ),
    "continuous"
  )

  .quantile_variables <- load_variables_file(
    get_variables_path(
      folder_path,
      continuous_quantiles_file,
      "quantiles"
    ),
    "quantiles"
  )

  if (! is_variables_valid(
    .binary_variables,
    .categorical_variables,
    .continuous_variables,
    .quantile_variables
  )) {
    stop("The input files are not valid for the RESIDE package")
  }

}
