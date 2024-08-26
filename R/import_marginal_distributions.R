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

  # Validate the variables and throw an error if they
  # are invalid.
  if (! is_variables_valid(
    .binary_variables,
    .categorical_variables,
    .continuous_variables,
    .quantile_variables
  )) {
    stop("The input files are not valid for the RESIDE package")
  }

}
