binary_df <- read.csv(
  testthat::test_path("testdata", "binary_variables.csv")
)

categorical_df <- read.csv(
  testthat::test_path("testdata", "categorical_variables.csv")
)

continuous_df <- read.csv(
  testthat::test_path("testdata", "continuous_variables.csv")
)

quantile_df <- read.csv(
  testthat::test_path("testdata", "continuous_quantiles.csv")
)

summary_df <- read.csv(
  testthat::test_path("testdata", "summary.csv")
)

empty_df <- data.frame()

marginal_distributions <- get_marginal_distributions(
  IST,
  variables = c(
    "SEX",
    "AGE",
    "ID14",
    "RSBP",
    "RATRIAL",
    "SET14D"
  )
)

# Quck and dirty random string function
random_string <- function() {
  return(
    runif(1, 1000000000000, 9999999999999) %>% round %>% as.character
  )
}

# Function to return a new directory in the temporary directory
new_temp_dir <- function() {
  .temp_dir <- file.path(
    tempdir(),
    random_string()
  )
  dir.create(.temp_dir)
  return(
    .temp_dir
  )
}