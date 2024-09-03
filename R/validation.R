is_variables_valid <- function(
  binary_variables,
  categorical_variables,
  continuous_variables,
  quantile_variables,
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
      quantile_variables,
      "quantile"
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
  if (!all(
    continuous_variables$variable %in%
      gsub("_q$", "", levels(as.factor(quantile_variables$varname)))
  )) {
    # Produce a helpful message
    cat(
      "Continuous variables do not match quantiles"
    )
    # Return FALSE
    return(FALSE)
  }
  # If all variables pass checks return TRUE
  return(TRUE)
}

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
      cat(
        variable_type,
        "not valid \n"
      )
      # And return FALSE
      return(FALSE)
    }
  }
  # If all columns are present return TRUE
  return(TRUE)
}

get_required_variables <- function(
  variable_type
) {
  # Return the required columns for a given type
  # Use a list to allow uneven vectors in case_when
  return(
    dplyr::case_when(
      variable_type == "binary" ~
        list(c("variable", "mean")),
      variable_type == "categorical" ~
        list(c("category", "n", "variable")),
      variable_type == "continuous" ~
        list(c("variable", "m", "s", "missing")),
      variable_type == "quantile" ~
        list(c("varname", "orig_q", "tform_q", "epsilon")),
      variable_type == "summary" ~
        list(c("n_row", "n_col", "variables")),
      .default = list(c("ERROR", "UNKNOWN VARIABLE"))
    )
  )
}