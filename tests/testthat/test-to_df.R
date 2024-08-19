test_reside <- get_marginal_distributions(
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

testthat::test_that("binary_to_df works", {
  binary_df <- binary_to_df(
    test_reside$binary_variables
  )
  testthat::expect_s3_class(binary_df, "data.frame")
  testthat::expect_true(
    all(
      c("variable", "mean") %in% names(binary_df)
    )
  )
})

testthat::test_that("quantiles_to_df works", {
  quantiles_df <- quantiles_to_df(
    test_reside$continuous_variables
  )
  testthat::expect_s3_class(quantiles_df, "data.frame")
  testthat::expect_true(
    all(
      c("varname", "orig_q", "tform_q", "epsilon") %in% names(quantiles_df)
    )
  )
})