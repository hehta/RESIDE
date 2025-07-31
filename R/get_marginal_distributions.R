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
  # Copy the original df
  original_df <- df
  .return <- list()

  # Handle lists of data frames
  if (is.list(df) && !is.data.frame(df)) {
    # Check if any of the data frames are long format
    if (any(unlist(lapply(
      df, is_long_format, subject_identifier = subject_identifier
    )))) {
      .return <- .get_long_summaries(
        df,
        subject_identifier
      )
      .return$summary <- .add_n_row_summaries(
        original_df,
        .return$summary
      )
    } else {
      # If not long format, just join the data frames
      dfs <- lapply(
        df,
        .prepare_df,
        subject_identifier = subject_identifier,
        variables = variables,
        retype = retype
      )
      df <- .join_df(
        dfs,
        subject_identifier,
        unique(df[[subject_identifier]])
      )
      df <- .remove_subject_identifier(
        df,
        subject_identifier
      )
      .return <- .get_summaries(
        df,
        subject_identifier
      )
    }
    .return$summary <- .add_n_row_summaries(
      original_df,
      .return$summary
    )
  } else {
    # If df is not a list, prepare the data frame
    df <- .prepare_df(
      df,
      subject_identifier = subject_identifier,
      variables = variables,
      retype = retype
    )
    df <- .remove_subject_identifier(
      df,
      subject_identifier
    )
    # Get the summaries
    .return <- .get_summaries(
      df,
      subject_identifier
    )
  }

  # Add a class to the return to allow for S3 overrides
  class(.return) <- "RESIDE"

  # If print is TRUE print the marginal distributions
  if (print) {
    print(.return)
  }

  # Return the S3 Class
  return(
    .return
  )

}

.prepare_df <- function(
  df,
  subject_identifier = "",
  variables = c(),
  retype = TRUE
) {

  # Check if subject identifier is set
  if (!is.character(subject_identifier)) {
    stop("Subject identifier must be a character")
  }

  # Check if variables is set
  if (length(variables) > 0) {
    if (!is.character(variables)) {
      stop("Variables must be a vector of characters")
    }
    # Error is any variables are missing
    .missing_variables <- get_missing_variables(df, variables)
    if (length(.missing_variables) > 0) {
      stop(
        paste(
          "all variables must be in df missing:",
          .missing_variables,
          sep = " ",
          collapse = ", "
        )
      )
    }
    # Select only the variables in the data frame
    df <- df[variables]
  }

  # Re-type the data frame
  # if (retype) {
  #   df <- dplyr::mutate_if(df, is.character, as.factor)
  # }

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

.remove_subject_identifier <- function(
  df,
  subject_identifier = ""
) {
  # Check if subject identifier is set
  if (!is.character(subject_identifier)) {
    stop("Subject identifier must be a character")
  }
  if (subject_identifier == "") {
    return(df)
  }
  # Remove subject identifier from df
  if (subject_identifier %in% names(df)) {
    df[[subject_identifier]] <- NULL
  }
  return(df)
}

.get_summaries <- function(
  df,
  subject_identifier = "",
  long_key = ""
) {
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
    .binary_summary[[paste0(.column, long_key)]] <- list(
      mean = mean(df[[.column]]),
      missing = get_n_missing(df, .column)
    )
  }

  # Forward declare categorical summary as empty list
  .categorical_summary <- list()
  # Loop through categorical variables
  for (.column in .categorical_variables) {
    # add (factor) summary to categorical summary
    .categorical_summary[[paste0(.column, long_key)]] <- summary(df[[.column]])
  }

  # Forward declare continuous summary as empty list
  .continuous_summary <- list()
  # Loop through continuous variables
  for (.column in .continuous_variables) {
    # Store the continuous variable in a temporary column
    .tmp_column <- df[.column]
    # Rename the temporary column to include the long key
    names(.tmp_column) <- paste0(.column, long_key)
    .continuous_summary[[paste0(.column, long_key)]] <- get_continuous_summary(
      .tmp_column
    )
  }

  .overall_summary <- data.frame(
    n_row = nrow(df),
    n_col = ncol(df),
    variables = paste(names(df), collapse = ", "),
    subject_identifier = subject_identifier
  )

  # Declare Return as a List
  return(
    list(
      categorical_variables = .categorical_summary,
      binary_variables = .binary_summary,
      continuous_variables = .continuous_summary,
      summary = .overall_summary
    )
  )
}

.get_long_summaries <- function(
  df,
  subject_identifier = ""
) {
  if (! is.list(df)) {
    df <- list(df)
  }
  keys <- names(df)
  if (is.null(keys)) {
    keys <- seq_len(length(df))
  }
  .summaries <- list()
  .df_summaries <- list()
  .wide_dfs <- list()
  for (key in keys) {
    .df <- df[[key]]
    if (!subject_identifier %in% names(.df)) {
      stop(
        "Subject identifier must be in all df's"
      )
    }
    .df <- .prepare_df(
      .df,
      subject_identifier = subject_identifier
    )
    long_columns <- get_long_columns(
      .df,
      subject_identifier
    )
    wide_columns <- setdiff(
      names(.df),
      long_columns
    )
    .wide_dfs[[key]] <- .df[wide_columns]
    .summaries[[key]] <- .get_summaries(
      .df[long_columns],
      subject_identifier,
      long_key = paste0(".df.", key)
    )
    .df_summaries[paste0("variables.df.", key)] <-
      paste(names(.df), collapse = ", ")
  }
  .baseline_df <- .list_to_df(
    .wide_dfs,
    subject_identifier
  )
  .baseline_df <- .prepare_df(
    .baseline_df
  )
  .baseline_df <- .remove_subject_identifier(
    .baseline_df,
    subject_identifier
  )
  .baseline_summaries <- .get_summaries(
    .baseline_df
  )
  .return_summaries <- .baseline_summaries
  for (.summary in .summaries) {
    .return_summaries <- .join_summaries(
      .return_summaries,
      .summary
    )
  }
  .return_summaries$summary <- data.frame(
    n_row = nrow(.baseline_df),
    n_col = ncol(.baseline_df),
    variables = paste(names(.baseline_df), collapse = ", "),
    subject_identifier = subject_identifier
  )
  .return_summaries$summary <- cbind(
    .return_summaries$summary,
    as.data.frame(.df_summaries)
  )
  return(
    .return_summaries
  )

}

.join_summaries <- function(
  summary_1,
  summary_2
) {
  summary_names <- c(names(summary_1), names(summary_2))
  summary_names <-
    summary_names[
      summary_names %in%
      c("categorical_variables", "binary_variables", "continuous_variables")
    ]
  summary_names <-
    unique(summary_names)
  .summary <- list()
  for (name in summary_names) {
    .summary[[name]] <- c(
      summary_1[[name]],
      summary_2[[name]]
    )
  }
  return(.summary)
}

.list_to_df <- function(
  df,
  subject_identifier
) {
  .subjects <- NULL
  # Check if subject_identifier is set
  if (subject_identifier == "") {
    stop("Subject identifier must be set")
  }
  # Check if subject_identifier is a character
  if (!is.character(subject_identifier)) {
    stop("Subject identifier must be a character")
  }

  .dfs <- list()
  if (!is.list(df)) {
    df <- list(df)
  }

  keys <- names(df)
  if (is.null(keys)) {
    keys <- seq_len(length(df))
  }

  for (key in keys) {
    .df <- df[[key]]
    if (!subject_identifier %in% names(.df)) {
      stop(
        "Subject identifier must be in all df's"
      )
    }
    if (is_long_format(.df, subject_identifier)) {
      .df <- long_to_wide(
        .df,
        subject_identifier
      )
    }
    # Sanity check that the subject identifier is a unique
    if (
      length(unique(.df[[subject_identifier]])) != nrow(dplyr::distinct(.df))
    ) {
      stop(
        "Subject identifier must be unique in all df's"
      )
    }
    if (is.null(.subjects)) {
      .subjects <- unique(.df[[subject_identifier]])
    } else {
      .subjects <- c(
        .subjects,
        unique(.df[[subject_identifier]])
      )
    }
    .dfs[[key]] <- .df
  }
  return(
    .join_df(
      .dfs,
      subject_identifier,
      unique(.subjects)
    )
  )
}

.join_df <- function(
  df,
  subject_identifier,
  unique_subjects
) {
  # loop through df's
  dfs <- df
  output_df <- as.data.frame(
    list(unique_subjects),
    col.names = subject_identifier
  )
  if (!is.list(df)) {
    dfs <- list(dfs)
  }
  keys <- names(dfs)
  if (is.null(keys)) {
    keys <- seq_len(length(dfs))
  }
  prev_key <- 0
  unmatched_cols <- c()
  for (key in keys){
    output_df <- dplyr::full_join(
      output_df,
      dfs[[key]],
      by = subject_identifier,
      suffix = c(
        paste0(".df.", prev_key),
        paste0(".df.", key)
      )
    )
    prev_key <- key
    for (unmatched_col in unmatched_cols) {
      print(unmatched_col)
      raw_unmatched_col <- gsub("\\.df\\..+", "", unmatched_col)
      if (raw_unmatched_col %in% names(output_df)) {
        names(output_df)[names(output_df) == raw_unmatched_col] <-
          paste0(raw_unmatched_col, ".df.", key)
      }
    }
    unmatched_cols <- c(
      unmatched_cols,
      names(output_df)[grepl("\\.df\\.", names(output_df))]
    )
  }
  if (any(grepl("\\.df\\.", names(output_df)))) {
    warning(
      "Data frames contain columns with the same name,
      but different values."
    )
  }
  output_df <- output_df[!duplicated(output_df[[subject_identifier]]), ]
  return(output_df)
}

get_variable_types <- function(df) {
  # Declare variables
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
  return(list(
    categorical_variables = .categorical_variables,
    continuous_variables = .continuous_variables,
    binary_variables = .binary_variables
  ))
}

generate_variables_list <- function(dfs) {
  if (!is.list(dfs)) {
    dfs <- list(dfs)
  }
  keys <- names(dfs)
  variables <- list()
  if (is.null(keys)) {
    keys <- seq_along(dfs)
  }
  for (key in keys){
    variables[paste0("variable.df.", key)] <-
      paste0(names(dfs[[key]]), collapse = ", ")
  }
  return(as.data.frame(variables))
}

.add_n_row_summaries <- function(
  dfs,
  summary_df
) {
  keys <- names(dfs)
  if (is.null(keys)) {
    keys <- seq_along(dfs)
  }
  for (key in keys){
    column_name <- paste0("n_row.df.", key)
    n_row <- nrow(dfs[[key]])
    summary_df[, column_name] <- n_row
  }
  return(summary_df)
}

.get_variable_names  <- function(
  summary_list
) {
  # Check if variable_type is valid
  variable_types <- c("categorical", "binary", "continuous")

  variable_types <- names(summary_list)[names(summary_list) %in% variable_types]
  if (length(variable_types) == 0) {
    stop("No valid variable types found in summary_list")
  }
  variable_names <- c()
  for (variable_type in variable_types) {
    if (variable_type %in% names(summary_list)) {
      variable_names <- c(
        variable_names,
        names(summary_list[[variable_type]])
      )
    }
  }
  return(variable_names)
}