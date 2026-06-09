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
  if (missing(folder_path)) {
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
  # Forward declaration vectors for each type of marginal distribution
  categorical_dfs <- list()
  binary_dfs <- list()
  continuous_quantile_dfs <- list()
  summary_dfs <- list()
  # Make a copy of the marginals
  marginals_copy <- marginals
  # Remove overall summary if it exists (it is not a marginal distribution)
  if ("overall_summary" %in% names(marginals)) {
    marginals_copy[["overall_summary"]] <- NULL
  }
  df_names <- get_df_names_or_key(marginals_copy)
  for (i in df_names) {
    df_marginals <- marginals_copy[[i]]

    if ("categorical_variables" %in% names(df_marginals)) {
      # Convert the marginals to a data frame
      if (length(df_marginals$categorical_variables) > 0) {
        .categorical_df <- categorical_to_df(df_marginals$categorical_variables)
        .categorical_df[["data_frame"]] <- i
        categorical_dfs <- append(categorical_dfs, list(.categorical_df))
      }
    }

    # Check there are categorical variables
    if ("binary_variables" %in% names(df_marginals)) {
      if (length(df_marginals$binary_variables) > 0) {
        .binary_df <- binary_to_df(marginals$binary_variables)
        .binary_df[["data_frame"]] <- i
        binary_dfs <- append(binary_dfs, list(.binary_df))
      }
    }

    if ("continuous_variables" %in% names(df_marginals)) {
      if (length(df_marginals$continuous_variables) > 0) {
        # Convert the marginals to a data frame
        .continuous_df <- continuous_to_df(df_marginals$continuous_variables)

        # Convert the quantiles to a data frame
        .quantiles_df <- quantiles_to_df(df_marginals$continuous_variables)
        .quantiles_df[["data_frame"]] <- i

        .quantiles_df[["is_date"]] <- .continuous_df$is_date[match(
          .quantiles_df$variable,
          .continuous_df$variable
        )]

        .quantiles_df[["missing"]] <- .continuous_df$missing[match(
          .quantiles_df$variable,
          .continuous_df$variable
        )]

        .quantiles_df[["max_dp"]] <- .continuous_df$max_dp[match(
          .quantiles_df$variable,
          .continuous_df$variable
        )]

        # Add to vector
        continuous_quantile_dfs <- append(
          continuous_quantile_dfs,
          list(.quantiles_df)
        )
      }
    }

    if ("summary" %in% names(df_marginals)) {
      .summary_df <- df_marginals$summary

      # Add to vector if there are any rows
      if (nrow(.summary_df) > 0) {
        .summary_df[["data_frame"]] <- i
        summary_dfs <- append(summary_dfs, list(.summary_df))
      }
    }
  }

  if (length(categorical_dfs) > 0) {
    categorical_df <- do.call(rbind, categorical_dfs)
    .write_csv(
      categorical_df,
      file.path(folder_path, "categorical_variables.csv"),
      "Categorical variables"
    )
  }

  if (length(binary_dfs) > 0) {
    binary_df <- do.call(rbind, binary_dfs)
    .write_csv(
      binary_df,
      file.path(folder_path, "binary_variables.csv"),
      "Binary variables"
    )
  }

  if (length(continuous_quantile_dfs) > 0) {
    continuous_quantiles_df <- do.call(rbind, continuous_quantile_dfs)
    .write_csv(
      continuous_quantiles_df,
      file.path(folder_path,
        "continuous_variables.csv"
      ),
      "Continuous variables"
    )
  }

  if (length(summary_dfs) > 0) {
    summary_df <- do.call(rbind, summary_dfs)
    summary_df$common_columns <- ifelse(
      "overall_summary" %in% names(marginals),
      marginals$overall_summary$common_columns,
      ""
    )
    summary_df$n_subjects <- ifelse(
      "overall_summary" %in% names(marginals),
      marginals$overall_summary$n_subjects,
      ""
    )
    .write_csv(
      summary_df,
      file.path(folder_path, "summary.csv"),
      "Summary",
      row_names = FALSE
    )
  }
  invisible(NULL)
}