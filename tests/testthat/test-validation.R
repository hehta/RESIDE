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
  testthat::expect_message(
    is_variable_valid(quantile_df, "continuous"),
    regexp = "^continuous not valid.+$"
  )
  testthat::expect_message(
    is_variable_valid(binary_df, "categorical"),
    regexp = "^categorical not valid.+$"
  )
  testthat::expect_message(
    is_variable_valid(continuous_df, "quantile"),
    regexp = "^quantile not valid.+$"
  )
  testthat::expect_message(
    is_variable_valid(categorical_df, "binary"),
    regexp = "^binary not valid.+$"
  )
  testthat::expect_message(
    is_variable_valid(categorical_df, "summary"),
    regexp = "^summary not valid.+$"
  )
})

testthat::test_that("is_variables_valid works", {
  # Test true returned when all valid
  expect_true(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      quantile_df,
      summary_df
    )
  )

  # Test TRUE on empty (Not summary)
  expect_true(
    is_variables_valid(
      data.frame(),
      data.frame(),
      data.frame(),
      data.frame(),
      summary_df
    )
  )

  # Test FALSE on empty (summary)
  expect_true(
    is_variables_valid(
      data.frame(),
      data.frame(),
      data.frame(),
      data.frame(),
      data.frame()
    )
  )

  # Expect FALSE when invalid
  expect_false(
    is_variables_valid(
      summary_df,
      categorical_df,
      continuous_df,
      quantile_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      continuous_df,
      continuous_df,
      quantile_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      binary_df,
      quantile_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      continuous_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      quantile_df,
      continuous_df
    )
  )

  # Expect FALSE when continuous and quantiles don't match
  quantile_df_rm <- quantile_df
  quantile_df_rm <- quantile_df_rm[1:16, ]

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      quantile_df_rm,
      summary_df
    )
  )

  testthat::expect_message(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      quantile_df_rm,
      summary_df
    ),
    regexp = "^.*Continuous variables do not match quantiles.*$"
  )

})