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

marginal_distibutions <- get_marginal_distributions(
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