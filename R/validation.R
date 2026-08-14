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
  # remove the common variables
  variables_test <- setdiff(all_marginal_variables, common_variables)
  # Get the duplicated variables
  duplicated_variables <- variables_test[duplicated(variables_test)]
  # Ingoring common variables, ignore variables that are not duplicated
  # as these do not need a data frame name.
  if (length(duplicated_variables) == 0) {
    return(TRUE)
  }
  # Get all the correlation variables
  correlation_variables <- get_correlation_names(correlations)
  matching_variables <-
    correlation_variables[correlation_variables %in% duplicated_variables]
  # Only continue if there are any correlation variables that
  # are duplicated in the marginals
  if (! length(matching_variables) == 0) {
    return(TRUE)
  }
  # loop through the correlations to check the data frame names are present
  # for the duplicated variables
  for (correlation in correlations) {
    # Forward declare the df_name and correlation_name variables
    df_name <- ""
    correlation_name <- ""
    # Set the df_name based on either the x or y variable
    # being present in the matching variables and their corresponding df_name
    # being present in the correlation object. Or if the df_name
    # is present in the correlation object.
    if (correlation$x %in% matching_variables) {
      correlation_name <- correlation$x
      if ("df_name" %in% names(correlation)) {
        df_name <- correlation$df_name
      }
      if ("df_name.x" %in% names(correlation)) {
        df_name <- correlation$df_name.x
      }
    } else if (correlation$y %in% matching_variables) {
      correlation_name <- correlation$y
      if ("df_name" %in% names(correlation)) {
        df_name <- correlation$df_name
      }
      if ("df_name.y" %in% names(correlation)) {
        df_name <- correlation$df_name.y
      }
    }
    # if the df_name is not found, then no data frame name was specified
    #for a variable that is duplicated in the marginals
    if (df_name == "") {
      stop(
        paste(
          "Correlation variable",
          correlation_name,
          "is duplicated in the marginals and 
          must have a data frame name specified."
        )
      )
    }
    validate_df_name(marginals, correlation_name, df_name)
    return(TRUE)
  }


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
      if (factor_name %in% sub_marginals$categorical_variables[[variable]]$category) { #nolint: line_length_linter
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
  correlation_factors <- get_correlation_factors(correlations)
  # Check the correlation factors are valid
  check_variables_exist(marginals, correlation_factors)
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

