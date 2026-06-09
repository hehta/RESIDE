##################################################################
##                     Validation Functions                     ##
##################################################################
# Check that variables are valid
is_variables_valid <- function(
  binary_variables,
  categorical_variables,
  continuous_variables,
  summary_variables
) {
  # Check all of the variables
  if (!all(
    is_variable_valid(
      binary_variables,
      "binary"
    ),
    is_variable_valid(
      categorical_variables,
      "categorical"
    ),
    is_variable_valid(
      continuous_variables,
      "continuous"
    ),
    is_variable_valid(
      summary_variables,
      "summary"
    )
  )) {
    # Return FALSE if any variables aren't valid
    return(FALSE)
  }
  # Check the quantile names against the continuous variables
  if (!is_data_frames_valid(
    binary_variables,
    categorical_variables,
    continuous_variables,
    summary_variables
  )) {
    # Produce a helpful message
    message(
      "Date Frame names are missing from on or more files"
    )
    # Return FALSE
    return(FALSE)
  }
  # If all variables pass checks return TRUE
  return(TRUE)
}

# Wrapper function to check a variable is valid
# based on it's type.
is_variable_valid <- function(
  variable_df,
  variable_type
) {
  # Ignore if the data frame is empty
  if (ncol(variable_df) > 0) {
    # Check all the required columns are present in data frame
    if (!all(
      get_required_variables(variable_type)[[1]] %in% names(variable_df)
    )) {
      # Else produce a handy message
      message(
        paste(
          variable_type,
          "not valid"
        )
      )
      # And return FALSE
      return(FALSE)
    }
  }
  # If all columns are present return TRUE
  return(TRUE)
}

# Function to return the variable names as a list
# for a given variable type.
get_required_variables <- function(
  variable_type
) {
  # Return the required columns for a given type
  # Use a list to allow uneven vectors in case_when
  return(
    dplyr::case_when(
      variable_type == "binary" ~
        list(c("variable", "mean", "missing")),
      variable_type == "categorical" ~
        list(c("category", "n", "variable")),
      variable_type == "continuous" ~
        list(c(
          "variable",
          "orig_q",
          "tform_q",
          "epsilon",
          "is_date",
          "missing",
          "max_dp"
        )),
      variable_type == "summary" ~
        list(c("n_row", "n_col", "variables")),
      .default = list(c("ERROR", "UNKNOWN VARIABLE"))
    )
  )
}

is_data_frames_valid <- function(
  binary_variables,
  categorical_variables,
  continuous_variables,
  summary_variables
) {
  variable_dfs <- list(
    binary_variables,
    categorical_variables,
    continuous_variables,
    summary_variables
  )
  n_variable_types <- lapply(
    variable_dfs,
    function(x) ifelse(nrow(x) > 0, 1L, 0L)
  )
  n_variable_types <- do.call(sum, n_variable_types)
  n_dfs <- lapply(
    variable_dfs,
    function(x) ifelse("data_frame" %in% names(x), 1L, 0L)
  )
  n_dfs <- do.call(sum, n_dfs)
  n_dfs == n_variable_types
}

check_cor_variables <- function(
  variables,
  marginals
) {
  # Get all the names in the marginals
  full_names <- get_variables(marginals)
  # Convert the names to lower case
  col_names <- tolower(full_names)

  # remove the keys
  for (key in multi_keys) {
    col_names <- gsub(key, "", col_names)
  }

  # Convert the variables to lowercase
  variables <- tolower(variables)

  # Get the matches between the column names and variables
  matches <- col_names %in% variables
  # Get the matches between the variables and columns
  reverse_matches <- variables %in% col_names
  # Check all variables are in the marginals
  if (!all(reverse_matches)) {
    stop(
      paste0("Error the following variables were not found: ",
             paste0(variables[!reverse_matches], collapse = ", "))
    )
  }
  # Check there are no variables that need a df key that don't have one.
  for (variable in variables) {
    v_matches <- col_names %in% variable
    if (length(v_matches) > 1) {
      if (!variable %in% tolower(full_names))
        stop(
          paste0("More than one variable found with name: ",
                 variable,
                 " please be more specific, options are: ",
                 paste0(full_names[v_matches], collapse = ", "))
        )
    }
  }
  return(TRUE)
}