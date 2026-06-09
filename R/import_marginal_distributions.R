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
    .summary_variables
  )) {
    stop("The input files are not valid for the RESIDE package")
  }

  df_names <- c()
  if ("data_frame" %in% names(.summary_variables)) {
    df_names <- unique(.summary_variables$data_frame)
  } else {
    df_names <- seq_len(nrow(.summary_variables))
  }

  .return <- list()

  for (df_name in df_names) {
    binary_variables <-
      .filter_by_df_name(.binary_variables, df_name)
    categorical_variables <-
      .filter_by_df_name(.categorical_variables, df_name)
    continuous_variables <-
      .filter_by_df_name(.continuous_variables, df_name)
    summary_variables <-
      .filter_by_df_name(.summary_variables, df_name)

    if ("common_columns" %in% names(summary_variables)) {
      summary_variables <- dplyr::select(
        summary_variables,
        -common_columns
      )
    }

    if ("n_subjects" %in% names(summary_variables)) {
      summary_variables <- dplyr::select(
        summary_variables,
        -n_subjects
      )
    }

    .return[[df_name]] <- list(
      categorical_variables = .gen_categorical_summary(
        categorical_variables
      ),
      binary_variables = .gen_binary_summary(
        binary_variables
      ),
      continuous_variables = .gen_continuous_summary(
        continuous_variables
      ),
      summary = summary_variables
    )

  }

  .return$overall_summary <- data.frame(
    n_data_frames = length(df_names),
    data_frame_names = paste(df_names, collapse = ", "),
    subject_identifier = ifelse(
      "subject_identifier" %in% names(.summary_variables),
      .summary_variables$subject_identifier[1],
      ""
    ),
    n_subjects = ifelse(
      "n_subjects" %in% names(.summary_variables),
      .summary_variables$n_subjects[1],
      ""
    ),
    common_columns = ifelse(
      "common_columns" %in% names(.summary_variables),
      .summary_variables$common_columns[1],
      ""
    )

  )

  # Add a class to the return to allow for S3 overrides
  class(.return) <- "RESIDE"

  # Return the list
  return(.return)

}

.filter_by_df_name <- function(
  df,
  df_name
) {
  if ("data_frame" %in% names(df)) {
    return(df[df$data_frame == df_name, ]) # nolint: return
  } else {
    return(df) # nolint: return
  }
}

.gen_categorical_summary <- function (
  categorical_variables
) {
  # Forward declare categorical summary list
  .categorical_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(categorical_variables$variable))) {
    # We required a named vector of int so store this
    n <- categorical_variables[categorical_variables$variable == variable, ]$n
    # Set the names of the vector
    names(n) <-
      categorical_variables[
        categorical_variables$variable == variable,
      ]$category
    # Add the values to the categorical list
    # Using the variable name as the key
    .categorical_summary[[variable]] <- n
  }

  .categorical_summary
}

.gen_binary_summary <- function(
  binary_variables
) {
  # Forward declare binary summary list
  .binary_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(binary_variables$variable))) {
    # Add the mean of the variable to the list
    # Using the variable name as the key
    .binary_summary[[variable]] <- list(
      mean = binary_variables[binary_variables$variable == variable, ]$mean,
      missing =
        binary_variables[binary_variables$variable == variable, ]$missing
    )
  }
  .binary_summary
}

.gen_continuous_summary <- function(
  continuous_variables
) {
  # Forward declare continuous summary list
  .continuous_summary <- list()
  # Loop through variables (use unique rather than levels to maintain order)
  for (variable in unique(as.factor(continuous_variables$variable))) {
    # Get the quantiles as a df for the current variable
    .quantile_df <- as.data.frame(continuous_variables[
      continuous_variables$variable == variable,
    ])
    rownames(.quantile_df) <- seq_len(nrow(.quantile_df))
    .continuous_summary[[variable]] <- list(
      "quantiles" = dplyr::select(
        .quantile_df,
        variable, # nolint: object_name
        orig_q, # nolint: object_name
        tform_q, # nolint: object_name
        epsilon # nolint: object_name
      ),
      "summary" = dplyr::select(
        .quantile_df,
        is_date,
        missing, # nolint: object_name
        max_dp # nolint: object_name
      )[1, ] # Take the first row as these values will be the same
    )
  }
  .continuous_summary
}