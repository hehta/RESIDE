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
      c("variable", "mean", "missing") %in% names(binary_df)
    )
  )
  testthat::expect_gt(nrow(binary_df), 1)

  expected_df <- data.frame() %>%
    tibble::rownames_to_column(var = "variable")

  testthat::expect_equal(
    binary_to_df(list()),
    expected_df
  )
})

testthat::test_that("quantiles_to_df works", {
  quantiles_df <- quantiles_to_df(
    test_reside$continuous_variables
  )
  testthat::expect_s3_class(quantiles_df, "data.frame")
  testthat::expect_true(
    all(
      c("variable", "orig_q", "tform_q", "epsilon") %in% names(quantiles_df)
    )
  )
  testthat::expect_gt(nrow(quantiles_df), 1)

  testthat::expect_equal(
    quantiles_to_df(list()),
    data.frame()
  )
})

testthat::test_that("continuous_to_df works", {
  continuous_df <- continuous_to_df(
    test_reside$continuous_variables
  )
  testthat::expect_s3_class(continuous_df, "data.frame")
  testthat::expect_true(
    all(
      c("variable", "mean", "sd", "missing", "max_dp") %in% names(continuous_df)
    )
  )
  testthat::expect_gt(nrow(continuous_df), 1)

  testthat::expect_equal(
    continuous_to_df(list()),
    data.frame(variable = c())
  )
})

testthat::test_that("categorical_to_df works", {
  categorical_df <- categorical_to_df(
    test_reside$categorical_variables
  )
  testthat::expect_s3_class(categorical_df, "data.frame")
  testthat::expect_true(
    all(
      c("category", "n", "variable") %in% names(categorical_df)
    )
  )
  testthat::expect_gt(nrow(categorical_df), 1)

  testthat::expect_equal(
    categorical_to_df(list()),
    data.frame(variable = c())
  )
})