#' @title summary.RESIDE
#' @description S3 override for summary RESIDE
#' @param object an object of class RESIDE
#' @param ... Other parameters currently none are used
#' @return An object of class summary.RESIDE, a list containing
#' \code{overall}, a data frame of the overall summary, and
#' \code{data_frames}, a data frame with a row for each data frame.
#' @details S3 Override for RESIDE Class, a higher level summary than
#' \code{\link{print.RESIDE}}. For each data frame it gives the number of
#' rows, subjects and variables, the number of each type of variable, the
#' number of date variables and the number of variables with missing data.
#' @examples
#' summary(
#'   get_marginal_distributions(
#'     IST,
#'     variables = c(
#'       "SEX",
#'       "AGE",
#'       "ID14",
#'       "RSBP",
#'       "RATRIAL"
#'     )
#'   )
#' )
#' @seealso
#'  \code{\link{print.RESIDE}}
#' @rdname summary.RESIDE
#' @export
#' @importFrom methods is
summary.RESIDE <- function(
  object,
  ...
) {
  # Check class
  if (!methods::is(object, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  overall_summary <- object[["overall_summary"]]
  df_names <- get_df_names_or_key(object)
  overall <- data.frame(
    n_data_frames = length(df_names),
    subject_identifier = overall_summary[["subject_identifier"]],
    n_subjects = overall_summary[["n_subjects"]],
    common_columns = overall_summary[["common_columns"]]
  )
  # Summarise each data frame
  data_frames <- do.call(
    rbind,
    lapply(df_names, function(df_name) {
      summarise_sub_marginals(object[[df_name]], df_name)
    })
  )
  rownames(data_frames) <- NULL
  structure(
    list(
      overall = overall,
      data_frames = data_frames,
      single_table = is_single_table(object)
    ),
    class = "summary.RESIDE"
  )
}

#' @title print.summary.RESIDE
#' @description S3 override for print summary.RESIDE
#' @param x an object of class summary.RESIDE
#' @param ... Other parameters currently none are used
#' @return The summary.RESIDE object, invisibly.
#' Called to print to the terminal.
#' @details S3 Override for summary.RESIDE Class, prints the overall
#' summary followed by a table summarising each data frame.
#' @seealso
#'  \code{\link{summary.RESIDE}}
#' @rdname print.summary.RESIDE
#' @export
print.summary.RESIDE <- function(
  x,
  ...
) {
  overall <- x[["overall"]]
  cat("Summary of Marginal Distributions\n")
  if (!x[["single_table"]]) {
    cat("Number of Data Frames:", overall[["n_data_frames"]], "\n")
  }
  if (overall[["subject_identifier"]] != "") {
    cat("Subject Identifier:", overall[["subject_identifier"]], "\n")
  }
  cat("Number of Subjects:", overall[["n_subjects"]], "\n")
  if (!x[["single_table"]] && overall[["common_columns"]] != "") {
    cat("Common Columns:", overall[["common_columns"]], "\n")
  }
  cat("\n")
  data_frames <- x[["data_frames"]]
  # The data frame name is not needed for a single table
  if (x[["single_table"]]) {
    data_frames[["data_frame"]] <- NULL
  }
  # Use readable column names
  labels <- c(
    data_frame = "Data Frame",
    n_row = "Rows",
    n_subjects = "Subjects",
    n_variables = "Variables",
    n_categorical = "Categorical",
    n_binary = "Binary",
    n_continuous = "Continuous",
    n_dates = "Dates",
    n_missing = "Missing"
  )
  names(data_frames) <- labels[names(data_frames)]
  print(data_frames, row.names = FALSE)
  invisible(x)
}

# Internal function to summarise the marginals of a single data frame
summarise_sub_marginals <- function(
  sub_marginals,
  df_name
) {
  categorical_variables <- sub_marginals[["categorical_variables"]]
  binary_variables <- sub_marginals[["binary_variables"]]
  continuous_variables <- sub_marginals[["continuous_variables"]]
  # Categorical variables are missing if they have a missing category
  categorical_missing <- vapply(
    categorical_variables,
    function(categories) {
      any(categories[names(categories) %in% c("missing", "NA's")] > 0)
    },
    logical(1)
  )
  binary_missing <- vapply(
    binary_variables,
    function(variable) variable[["missing"]] > 0,
    logical(1)
  )
  continuous_missing <- vapply(
    continuous_variables,
    function(variable) variable[["summary"]][["missing"]] > 0,
    logical(1)
  )
  is_date <- vapply(
    continuous_variables,
    function(variable) isTRUE(variable[["summary"]][["is_date"]]),
    logical(1)
  )
  data.frame(
    data_frame = as.character(df_name),
    n_row = sub_marginals[["summary"]][["n_row"]],
    n_subjects = get_sub_n_subjects(sub_marginals),
    n_variables = length(get_submarginal_variables(sub_marginals)),
    n_categorical = length(categorical_variables),
    n_binary = length(binary_variables),
    n_continuous = length(continuous_variables),
    n_dates = sum(is_date),
    n_missing = sum(categorical_missing, binary_missing, continuous_missing)
  )
}
