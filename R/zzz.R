# Allow lazy evaluation.
utils::globalVariables(c(
  "orig_q",
  "tform_q",
  "epsilon",
  "mean",
  "sd",
  "n_row",
  "n_col",
  "variables",
  "max_dp"
))

# Global filenames
.marginal_file_names <- c(
  "binary_variables.csv",
  "categorical_variables.csv",
  "continuous_variables.csv",
  "continuous_quantiles.csv",
  "summary.csv"
)