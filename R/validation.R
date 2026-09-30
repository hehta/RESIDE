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

##################################################################
##                    Correlation Validation                    ##
##################################################################
validate_correlations <- function(
  marginals,
  correlations
) {
  variables <- get_correlation_names(correlations)
  # Check the correlation variables are valid
  check_variables_exist(marginals, variables)
  check_correlation_dfs(marginals, correlations)
  check_factor_correlations(marginals, correlations)
}

check_variables_exist <- function(
  marginals,
  variables
) {
  # Get the variables from the marginals
  marginals_variables <- get_variables(marginals)
  # Check the correlation variables are valid
  if (!all(variables %in% marginals_variables)) {
    stop(
      paste(
        "The following variables are not present in the marginals:",
        paste(variables[!variables %in% marginals_variables], collapse = ", ")
      )
    )
  }
}

# Check that the correlations have a data frame name if
# they are not common to all data frames or a single data frame.
check_correlation_dfs <- function(
  marginals,
  correlations
) {
  all_marginal_variables <- get_variables(
    marginals, unique = FALSE
  )
  # Get the common variables from the marginals
  common_variables <- .get_common_fields(marginals)
  # Get the duplicated variables, ignoring the common variables
  duplicated_variables <- setdiff(
    all_marginal_variables[duplicated(all_marginal_variables)],
    common_variables
  )
  # Get the variables that only appear in one data frame
  single_variables <- setdiff(
    all_marginal_variables,
    c(duplicated_variables, common_variables)
  )
  variables_by_df_name <- .get_variables_by_df_name(marginals)
  # loop through the correlations to check the data frame names are present
  # for the duplicated variables
  for (correlation in correlations) {
    for (var in c("x", "y")) {
      variable <- correlation[[var]]
      # Set the df_name from either the shared df_name
      # or the variable specific df_name (df_name.x / df_name.y)
      df_name <- ""
      if ("df_name" %in% names(correlation)) {
        df_name <- correlation$df_name
      }
      if (paste0("df_name.", var) %in% names(correlation)) {
        df_name <- correlation[[paste0("df_name.", var)]]
      }
      # Variables that only appear in one data frame do not need a
      # data frame name, warn if one is specified that doesn't match
      if (variable %in% single_variables) {
        variable_df_name <- names(variables_by_df_name)[
          vapply(
            variables_by_df_name,
            function(variables) variable %in% variables,
            logical(1)
          )
        ]
        if (df_name != "" && !identical(df_name, variable_df_name)) {
          warning(
            paste(
              "Data frame name",
              df_name,
              "will be ignored for correlation variable",
              variable,
              "as it only appears in one data frame."
            )
          )
        }
        next
      }
      if (! variable %in% duplicated_variables) {
        next
      }
      # if the df_name is not found, then no data frame name was specified
      # for a variable that is duplicated in the marginals
      if (df_name == "") {
        stop(
          paste(
            "Correlation variable",
            variable,
            "is duplicated in the marginals and",
            "must have a data frame name specified."
          )
        )
      }
      validate_df_name(marginals, variable, df_name)
    }
  }
  return(TRUE)
}

validate_df_name <- function(
  marginals,
  variable,
  df_name
) {
  variables_by_df_name <- .get_variables_by_df_name(marginals)
  if (! df_name %in% names(variables_by_df_name)) {
    stop(
      paste(
        "Data frame name",
        df_name,
        "is not present in the marginals."
      )
    )
  }
  if (! variable %in% variables_by_df_name[[df_name]]) {
    stop(
      paste(
        "Variable",
        variable,
        "is not present in the marginals for data frame",
        df_name
      )
    )
  }
}

check_factor_exists <- function(
  marginals,
  variable,
  factor_name,
  df_name = NULL
) {
  df_names <- get_df_names_or_key(marginals)
  factor_exists <- FALSE
  if (!is.null(df_name)) {
    df_names <- df_name
  }
  for (df in df_names) {
    sub_marginals <- marginals[[df]]
    if (! "categorical_variables" %in% names(sub_marginals)) {
      next
    }
    if (variable %in% names(sub_marginals$categorical_variables)) {
      if (factor_name %in% names(sub_marginals$categorical_variables[[variable]])) { #nolint: line_length_linter
        factor_exists <- TRUE
      }
    }
  }
  return(factor_exists)
}

check_factor_correlations <- function(
  marginals,
  correlations
) {
  # Check each factor exists for its variable
  # (the variables themselves are checked by validate_correlations)
  for (correlation in correlations) {
    for (var in c("x", "y")) {
      if (paste0("factor_name.", var) %in% names(correlation)) {
        factor_name <- correlation[[paste0("factor_name.", var)]]
        variable <- correlation[[var]]
        df_name <- NULL
        if ("df_name" %in% names(correlation)) {
          df_name <- correlation$df_name
        }
        if (paste0("df_name.", var) %in% names(correlation)) {
          df_name <- correlation[[paste0("df_name.", var)]]
        }
        if (!check_factor_exists(
          marginals,
          variable,
          factor_name,
          df_name
        )) {
          stop(
            paste(
              "Factor",
              factor_name,
              "is not present in the marginals for variable",
              variable
            )
          )
        }
      }
    }
  }
}



# Get the data frame (name or key) that a correlation variable belongs to,
# common variables return NA as they belong to every data frame.
get_correlation_df <- function(
  marginals,
  correlation,
  var,
  common_fields = .get_common_fields(marginals)
) {
  variable <- correlation[[var]]
  if (variable %in% common_fields) {
    return(NA)
  }
  df_names <- get_df_names_or_key(marginals)
  # Use the specified df_name if the variable is present in it
  df_name <- ""
  if ("df_name" %in% names(correlation)) {
    df_name <- correlation$df_name
  }
  if (paste0("df_name.", var) %in% names(correlation)) {
    df_name <- correlation[[paste0("df_name.", var)]]
  }
  if (
    df_name %in% df_names &&
      variable %in% get_submarginal_variables(marginals[[df_name]])
  ) {
    return(df_name)
  }
  # Otherwise use the data frame the variable is present in
  return(get_first_df(marginals, variable))
}

# Get the first data frame (name or key) that contains a variable
get_first_df <- function(
  marginals,
  variable
) {
  for (df in get_df_names_or_key(marginals)) {
    if (variable %in% get_submarginal_variables(marginals[[df]])) {
      return(df)
    }
  }
  stop(
    paste(
      "Correlation variable",
      variable,
      "is not present in the marginals."
    )
  )
}

# Check that categorical correlation variables have a factor name,
# as categorical variables are correlated using a dummy variable
# for a single category.
check_categorical_factor_names <- function(
  marginals,
  correlations
) {
  common_fields <- .get_common_fields(marginals)
  for (correlation in correlations) {
    for (var in c("x", "y")) {
      if (paste0("factor_name.", var) %in% names(correlation)) {
        next
      }
      variable <- correlation[[var]]
      df <- get_correlation_df(marginals, correlation, var, common_fields)
      if (is.na(df)) {
        df <- get_first_df(marginals, variable)
      }
      if (variable %in% names(marginals[[df]]$categorical_variables)) {
        stop(
          paste(
            "Correlation variable",
            variable,
            "is categorical and must have a factor name specified using",
            paste0("factor_name.", var)
          )
        )
      }
    }
  }
  return(TRUE)
}
