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

  

}