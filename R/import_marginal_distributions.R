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

  if (binary_variables_file == "") {
    .default_binary_variables_file <-
      file.path(
        normalizePath(folder_path),
        "binary_variables.csv"
      )
    if (file.exists(.default_binary_variables_file)) {
      binary_variables_file <- .default_binary_variables_file
    } else {
      message(
        paste0(
          "No Default Binary Variables File Present in",
          normalizePath(folder_path)
        )
      )
    }
  } else if (! file.exists(normalizePath(folder_path))) {
    stop("Binary File Must Exist")
  }
}
