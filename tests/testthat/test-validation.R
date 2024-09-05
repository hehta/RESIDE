testthat::test_that("get_required_variables_work", {
  # Expect Equal
  testthat::expect_equal(
    get_required_variables("binary"),
    list(c("variable", "mean", "missing"))
  )
  testthat::expect_equal(
    get_required_variables("continuous"),
    list(c("variable", "mean", "sd", "missing", "max_dp"))
  )
  testthat::expect_equal(
    get_required_variables("categorical"),
    list(c("category", "n", "variable"))
  )
  testthat::expect_equal(
    get_required_variables("quantile"),
    list(c("variable", "orig_q", "tform_q", "epsilon"))
  )
  testthat::expect_equal(
    get_required_variables("summary"),
    list(c("n_row", "n_col", "variables"))
  )
  testthat::expect_equal(
    get_required_variables("unknown"),
    list(c("ERROR", "UNKNOWN VARIABLE"))
  )
})

testthat::test_that("is_variable_valid works", {
  testthat::expect_true(
    is_variable_valid(binary_df, "binary")
  )
  testthat::expect_true(
    is_variable_valid(categorical_df, "categorical")
  )
  testthat::expect_true(
    is_variable_valid(continuous_df, "continuous")
  )
  testthat::expect_true(
    is_variable_valid(quantile_df, "quantile")
  )
  testthat::expect_true(
    is_variable_valid(summary_df, "summary")
  )
  testthat::expect_true(
    is_variable_valid(empty_df, "quantile")
  )
  testthat::expect_output(
    is_variable_valid(quantile_df, "continuous"),
    regexp = "^continuous not valid.+$"
  )
  testthat::expect_output(
    is_variable_valid(binary_df, "categorical"),
    regexp = "^categorical not valid.+$"
  )
  testthat::expect_output(
    is_variable_valid(continuous_df, "quantile"),
    regexp = "^quantile not valid.+$"
  )
  testthat::expect_output(
    is_variable_valid(categorical_df, "binary"),
    regexp = "^binary not valid.+$"
  )
  testthat::expect_output(
    is_variable_valid(categorical_df, "summary"),
    regexp = "^summary not valid.+$"
  )
})