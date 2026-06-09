#' @title Generate Marginal Distributions for a given data frame
#' @description Generate Marginal Distributions from a given
#' data frame with options to specify which variables to use.
#' @param df Data frame or a \code{"list"} of data frames
#' to get the marginal distributions from
#' @param subject_identifier (Optional) Subject identifier required if a
#' list of data frames is provided, Default: ""
#' @param variables (Optional) variable (columns) to select, Default: c()
#' @param print Whether to print the marginal distributions
#' to the console, Default: FALSE
#' @param retype Whether to re-type the data frame, Default: TRUE
#' @return A list of marginal distributions of an S3 RESIDE Class
#' @details A function to generate marginal distributions from
#' a given data frame, depending on the variable type the marginals
#' will differ, for binary variables a mean and number of missing is generated
#' for continuous variables, they are first transformed and both mean and sd of
#' the transformed variables are stored along with the quantile mapping for back
#' transformation. For categorical variables, the number of each category is
#' stored, missing values are categorise as "missing".
#' @examples
#' marginal_distributions <- get_marginal_distributions(
#'   IST,
#'   variables = c(
#'     "SEX",
#'     "AGE",
#'     "ID14",
#'     "RSBP",
#'     "RATRIAL"
#'   )
#' )
#' @seealso
#'  \code{\link{export_marginal_distributions}}
#' @rdname get_marginal_distributions
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
get_marginal_distributions <- function(
  df,
  subject_identifier = "",
  variables = c(),
  print = FALSE,
  retype = TRUE
) {
  # Copy the data fram to avoid confusion
  .df <- df
  .return <- list() # creae empty list to store the return value
  # Use a list of data frames if a single data frame is provided
  if (is.data.frame(.df)) {
    .df <- list(.df)
  }
  # Get the names of the data frames
  df_names <- get_df_names_or_key(.df)
  # Loop through data frames
  for (df_name in df_names) {
    # Get the data frame
    .current_df <- .df[[df_name]]
    # Prepare the data frame
    .current_df <- .prepare_df(
      .current_df,
      subject_identifier,
      variables,
      retype
    )
    # Get the summaries for the data frame and store in the return list
    .return[[df_name]] <- .get_summaries(.current_df, subject_identifier)
  }
  # Get the overall summary and store in the return list
  .return$overall_summary <- .get_overall_summary(.df, subject_identifier)
  # Add s3 class to the return list
  class(.return) <- "RESIDE"
  # Return the marginal distributions
  .return
}

# Internal function to get summaries for a given data frame
.get_summaries <- function(
  df,
  subject_identifier = ""
) {
  n_subjects <- ifelse(
    subject_identifier != "",
    length(unique(df[[subject_identifier]])),
    nrow(df)
  )
  # Remove subject identifier from df
  if (subject_identifier %in% names(df)) {
    df[[subject_identifier]] <- NULL
  }

  # Get variable types
  .variable_types <- get_variable_types(df)
  .categorical_variables <- .variable_types$categorical_variables
  .continuous_variables <- .variable_types$continuous_variables
  .binary_variables <- .variable_types$binary_variables

  # Forward declare binary summary as empty list
  .binary_summary <- list()
  # Loop through binary variables
  for (.column in .binary_variables) {
    # add mean of binary variable to binary summary
    .binary_summary[[.column]] <- list(
      mean = mean(df[[.column]]),
      missing = get_n_missing(df, .column)
    )
  }

  # Forward declare categorical summary as empty list
  .categorical_summary <- list()
  # Loop through categorical variables
  for (.column in .categorical_variables) {
    # add (factor) summary to categorical summary
    .categorical_summary[[.column]] <- summary(df[[.column]])
  }

  # Forward declare continuous summary as empty list
  .continuous_summary <- list()
  # Loop through continuous variables
  for (.column in .continuous_variables) {
    # Store the continuous variable in a temporary column
    .tmp_column <- df[.column]
    .continuous_summary[[.column]] <- get_continuous_summary(
      .tmp_column
    )
  }
  # Create a summary of the data frame
  .summary <- data.frame(
    n_row = nrow(df),
    n_col = ncol(df),
    variables = paste(names(df), collapse = ", "),
    subject_identifier = subject_identifier,
    n_subjects = n_subjects
  )

  # Declare Return as a List
  return(
    list(
      categorical_variables = .categorical_summary,
      binary_variables = .binary_summary,
      continuous_variables = .continuous_summary,
      summary = .summary
    )
  )
}

# Internal function to get variable types for a given data frame
get_variable_types <- function(df) {
  # Forward declare variables
  .categorical_variables <- c()
  .continuous_variables <- c()
  .binary_variables <- c()

  # Identify Variable types
  # Loop through Columns
  for (.column in names(df)) {
    # If factor its a categorical variable
    if (is.factor(df[[.column]])) {
      .categorical_variables <- c(
        .categorical_variables,
        .column
      )
      # if numeric it's either binary or continuous
    } else if (is.numeric(df[[.column]])) {
      # If between 0 and 1 it's binary
      if (
        min(df[[.column]], na.rm = TRUE) == 0 &&
          max(df[[.column]], na.rm = TRUE) == 1
      ) {
        .binary_variables <- c(
          .binary_variables,
          .column
        )
        # Otherwise continuous
      } else {
        .continuous_variables <- c(
          .continuous_variables,
          .column
        )
      }
      # If neither factor or numeric, throw an error
    } else {
      stop(
        paste(
          "Unknown Variable type for column",
          .column,
          sep = " "
        )
      )
    }
  }
  return(list( #nolint: return
    categorical_variables = .categorical_variables,
    continuous_variables = .continuous_variables,
    binary_variables = .binary_variables
  ))
}

# Internal function to prepare a data frame
.prepare_df <- function(
  df,
  subject_identifier = "",
  variables = c(),
  retype = TRUE
) {

  # Re-type the data frame
  # Currently this only converts date columns to numeric,
  # but in the future it could add more functionality
  if (retype) {
    df <- .convert_date_columns(df)
  }

  # Replace missing values for characters with "missing"
  df <- df %>% dplyr::mutate_if(
    is.character,
    function(x) ifelse(x == "", "missing", x)
  )

  df <- df %>% dplyr::mutate(
    dplyr::across(
      dplyr::where(function(x) all(is.na(x))), ~  "missing"
    )
  )

  # Ensure characters are factors
  df <- df %>% dplyr::mutate_if(is.character, factor)

  return(df)
}

.prepare_dfs <- function(
  dfs,
  subject_identifier,
  variables,
  retype
) {
  .dfs <- dfs

  # Check if subject identifier is a character
  if (!is.character(subject_identifier)) {
    stop("Subject identifier must be a character")
  }

  # Check if variables are set
  if (length(variables) > 0) {
    # Check if variables is a vector of characters
    if (!is.character(variables)) {
      stop("Variables must be a vector of characters")
    }
    # Get any missing variables from the data frame(s)
    .missing_variables <- get_missing_variables(.dfs, variables)
    # If there are any missing variables, throw an error
    if (length(.missing_variables) > 0) {
      stop(
        paste(
          "all variables must be in data missing:",
          .missing_variables,
          sep = " ",
          collapse = ", "
        )
      )
    }
    # Check if subject identifier is set
    if (subject_identifier != "") {
      # If so check if subject identifier is present in all data frames
      if (! is_subject_identifier(.dfs, subject_identifier)) {
        # If not throw an error
        stop(
          paste(
            "Subject identifier",
            subject_identifier,
            "must be present in all data frames"
          )
        )
      }
      # Add subject identifier to variables
      variables <- c(variables, subject_identifier)
    }
    # Ensure variables are unique
    variables <- unique(variables)
    # Select only the variables in the data frame
    .dfs <- filter_variables(.dfs, variables)

  }

  # Loop through data frames
  for (i in seq_along(.dfs)) {
    .dfs[[i]] <- .prepare_df(
      .dfs[[i]],
      subject_identifier,
      variables,
      retype
    )
  }
  return(.dfs) #nolint: return
}

is_subject_identifier <- function(
  dfs,
  subject_identifier
) {
  .present <- lapply(dfs, function(df) {
    subject_identifier %in% names(df)
  })
  all(unlist(.present))
}

.get_overall_summary <- function(dfs, subject_identifier) {
  df_names <- get_df_names_or_key(dfs)
  n_subjects <- 0
  common_columns <- ""
  if (subject_identifier == "") {
    n_subjects <- nrow(dfs[[1]])
  } else {
    n_subjects <- get_n_unique_subjects(dfs, subject_identifier)
    common_columns <- get_common_columns(dfs, subject_identifier)
  }
  data.frame(
    n_data_frames = length(dfs),
    data_frame_names = paste(df_names, collapse = ", "),
    subject_identifier = subject_identifier,
    n_subjects = n_subjects,
    common_columns = common_columns
  )
}