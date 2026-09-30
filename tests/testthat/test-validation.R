testthat::test_that("get_required_variables_work", {
  # Expect Equal
  testthat::expect_equal(
    get_required_variables("binary"),
    list(c("variable", "mean", "missing"))
  )
  testthat::expect_equal(
    get_required_variables("continuous"),
    list(c(
      "variable",
      "orig_q",
      "tform_q",
      "epsilon",
      "is_date",
      "missing",
      "max_dp"
    ))
  )
  testthat::expect_equal(
    get_required_variables("categorical"),
    list(c("category", "n", "variable"))
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
    is_variable_valid(summary_df, "summary")
  )
  testthat::expect_true(
    is_variable_valid(empty_df, "quantile")
  )
  testthat::expect_message(
    is_variable_valid(binary_df, "categorical"),
    regexp = "^categorical not valid.+$"
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
      summary_df
    )
  )

  # Test TRUE on empty (Not summary)
  expect_true(
    is_variables_valid(
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
      data.frame()
    )
  )

  # Expect FALSE when invalid
  expect_false(
    is_variables_valid(
      summary_df,
      categorical_df,
      continuous_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      continuous_df,
      continuous_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      binary_df,
      summary_df
    )
  )

  expect_false(
    is_variables_valid(
      binary_df,
      categorical_df,
      continuous_df,
      continuous_df
    )
  )

})

testthat::test_that("check_factor_exists works", {
  # Expect TRUE when factor exists
  expect_true(
    check_factor_exists(
      marginal_distributions,
      "SEX",
      "M"
    )
  )
  # Expect FALSE when factor doesn't exist
  expect_false(
    check_factor_exists(
      marginal_distributions,
      "SEX",
      "X"
    )
  )
})

testthat::test_that("check_factor_exists works with multiple tables", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  # Expect TRUE when factor exists in one of the tables
  expect_true(
    check_factor_exists(
      multitable_marginals,
      "SEX",
      "M"
    )
  )
  expect_true(
    check_factor_exists(
      multitable_marginals,
      "AESEV",
      "SEVERE"
    )
  )
  # Expect FALSE when factor doesn't exist
  expect_false(
    check_factor_exists(
      multitable_marginals,
      "SEX",
      "X"
    )
  )
  # Expect FALSE when variable doesn't exist in any table
  expect_false(
    check_factor_exists(
      multitable_marginals,
      "NOTAVARIABLE",
      "M"
    )
  )
  # Expect TRUE when factor exists in the specified table
  expect_true(
    check_factor_exists(
      multitable_marginals,
      "SEX",
      "M",
      df_name = "dm"
    )
  )
  # Expect FALSE when factor exists, but not in the specified table
  expect_false(
    check_factor_exists(
      multitable_marginals,
      "SEX",
      "M",
      df_name = "ae"
    )
  )
})

testthat::test_that("validate_df_name works", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  # Expect no error when variable exists in the specified table
  expect_no_error(
    validate_df_name(
      multitable_marginals,
      "SEX",
      "dm"
    )
  )
  expect_no_error(
    validate_df_name(
      multitable_marginals,
      "AESEV",
      "ae"
    )
  )
  # Expect no error for a common variable in each table
  for (df_name in c("dm", "cm", "ae")) {
    expect_no_error(
      validate_df_name(
        multitable_marginals,
        "STUDYID",
        df_name
      )
    )
  }
  # Expect error when table doesn't exist
  expect_error(
    validate_df_name(
      multitable_marginals,
      "SEX",
      "xx"
    ),
    regexp = "Data frame name xx is not present in the marginals.",
    fixed = TRUE
  )
  # Expect error when variable exists, but not in the specified table
  expect_error(
    validate_df_name(
      multitable_marginals,
      "SEX",
      "ae"
    ),
    regexp = "Variable SEX is not present in the marginals for data frame ae",
    fixed = TRUE
  )
  # Expect error when variable doesn't exist in any table
  expect_error(
    validate_df_name(
      multitable_marginals,
      "NOTAVARIABLE",
      "dm"
    ),
    regexp = "Variable NOTAVARIABLE is not present in the marginals for data frame dm", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when a table is specified for single table marginals
  expect_error(
    validate_df_name(
      marginal_distributions,
      "SEX",
      "dm"
    ),
    regexp = "Data frame name dm is not present in the marginals.",
    fixed = TRUE
  )
})

testthat::test_that("check_factor_correlations works", {
  # Expect no error when the factor exists
  expect_no_error(
    check_factor_correlations(
      marginal_distributions,
      list(
        correlation("SEX", "AGE", 0.3, factor_name.x = "M")
      )
    )
  )
  # Expect no error when factors exist for both variables
  expect_no_error(
    check_factor_correlations(
      marginal_distributions,
      list(
        correlation(
          "SEX",
          "RATRIAL",
          0.3,
          factor_name.x = "F",
          factor_name.y = "Y"
        )
      )
    )
  )
  # Expect no error when no factors are specified
  expect_no_error(
    check_factor_correlations(
      marginal_distributions,
      list(
        correlation("SEX", "AGE", 0.3)
      )
    )
  )
  # Expect error when the factor doesn't exist
  expect_error(
    check_factor_correlations(
      marginal_distributions,
      list(
        correlation("SEX", "AGE", 0.3, factor_name.x = "X")
      )
    ),
    regexp = "Factor X is not present in the marginals for variable SEX",
    fixed = TRUE
  )
  # Expect error when the y factor doesn't exist
  expect_error(
    check_factor_correlations(
      marginal_distributions,
      list(
        correlation("AGE", "RATRIAL", 0.3, factor_name.y = "X")
      )
    ),
    regexp = "Factor X is not present in the marginals for variable RATRIAL",
    fixed = TRUE
  )
})

testthat::test_that("check_factor_correlations works with multiple tables", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  # Expect no error when the factor exists in one of the tables
  expect_no_error(
    check_factor_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3, factor_name.x = "M")
      )
    )
  )
  # Expect no error when the factor exists in the specified tables
  expect_no_error(
    check_factor_correlations(
      multitable_marginals,
      list(
        correlation(
          "SEX",
          "AESEV",
          0.3,
          df_name.x = "dm",
          df_name.y = "ae",
          factor_name.x = "M",
          factor_name.y = "SEVERE"
        )
      )
    )
  )
  # Expect no error when the factor exists in the shared df_name
  expect_no_error(
    check_factor_correlations(
      multitable_marginals,
      list(
        correlation(
          "AESEV",
          "AESER",
          0.3,
          df_name = "ae",
          factor_name.x = "MILD",
          factor_name.y = "Y"
        )
      )
    )
  )
  # Expect error when the factor doesn't exist
  expect_error(
    check_factor_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3, factor_name.y = "X")
      )
    ),
    regexp = "Factor X is not present in the marginals for variable AESEV",
    fixed = TRUE
  )
  # Expect error when the factor exists, but not in the specified table
  expect_error(
    check_factor_correlations(
      multitable_marginals,
      list(
        correlation(
          "SEX",
          "AESEV",
          0.3,
          df_name.x = "ae",
          df_name.y = "ae",
          factor_name.x = "M"
        )
      )
    ),
    regexp = "Factor M is not present in the marginals for variable SEX",
    fixed = TRUE
  )
})

testthat::test_that("validate_correlations works", {
  # Expect no error for valid correlations
  expect_no_error(
    validate_correlations(
      marginal_distributions,
      list(
        correlation("SEX", "AGE", 0.3),
        correlation("RATRIAL", "RSBP", -0.2, factor_name.x = "Y")
      )
    )
  )
  # Expect error when a variable doesn't exist
  expect_error(
    validate_correlations(
      marginal_distributions,
      list(
        correlation("NOTAVARIABLE", "AGE", 0.3)
      )
    ),
    regexp = "The following variables are not present in the marginals: NOTAVARIABLE", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when a factor doesn't exist
  expect_error(
    validate_correlations(
      marginal_distributions,
      list(
        correlation("SEX", "AGE", 0.3, factor_name.x = "X")
      )
    ),
    regexp = "Factor X is not present in the marginals for variable SEX",
    fixed = TRUE
  )
  # Expect a warning for each variable when a df_name is specified
  # for a single table
  expect_warning(
    expect_warning(
      validate_correlations(
        marginal_distributions,
        list(
          correlation("SEX", "AGE", 0.3, df_name = "dm")
        )
      ),
      regexp = "Data frame name dm will be ignored for correlation variable AGE as it only appears in one data frame.", #nolint: line_length_linter
      fixed = TRUE
    ),
    regexp = "Data frame name dm will be ignored for correlation variable SEX as it only appears in one data frame.", #nolint: line_length_linter
    fixed = TRUE
  )
})

testthat::test_that("validate_correlations works with multiple tables", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  # Expect no error when variables are unique to a table
  expect_no_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3)
      )
    )
  )
  # Expect no error for a common variable without a df_name
  expect_no_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("STUDYID", "SEX", 0.3)
      )
    )
  )
  # Expect no error when a duplicated variable has a df_name
  expect_no_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation(
          "DOMAIN",
          "SEX",
          0.3,
          df_name.x = "ae",
          df_name.y = "dm"
        )
      )
    )
  )
  expect_no_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("DOMAIN", "AESEV", 0.3, df_name = "ae")
      )
    )
  )
  # Expect error when a duplicated variable has no df_name
  expect_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("DOMAIN", "SEX", 0.3)
      )
    ),
    regexp = "Correlation variable DOMAIN is duplicated in the marginals and must have a data frame name specified.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when only a later correlation is invalid
  expect_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3),
        correlation("AESER", "DOMAIN", 0.3)
      )
    ),
    regexp = "Correlation variable DOMAIN is duplicated in the marginals and must have a data frame name specified.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when the df_name doesn't exist
  expect_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation(
          "DOMAIN",
          "SEX",
          0.3,
          df_name.x = "xx",
          df_name.y = "dm"
        )
      )
    ),
    regexp = "Data frame name xx is not present in the marginals.",
    fixed = TRUE
  )
  # Expect error when a variable doesn't exist
  expect_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("NOTAVARIABLE", "SEX", 0.3)
      )
    ),
    regexp = "The following variables are not present in the marginals: NOTAVARIABLE", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when a factor doesn't exist in the specified table
  expect_error(
    validate_correlations(
      multitable_marginals,
      list(
        correlation(
          "DOMAIN",
          "SEX",
          0.3,
          df_name.x = "ae",
          df_name.y = "dm",
          factor_name.x = "DM"
        )
      )
    ),
    regexp = "Factor DM is not present in the marginals for variable DOMAIN",
    fixed = TRUE
  )
  # Expect no warning when a df_name matches the only table of a variable
  expect_no_warning(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3, df_name.x = "dm", df_name.y = "ae")
      )
    )
  )
  # Expect no warning when a df_name is specified for a common variable
  expect_no_warning(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("STUDYID", "SEX", 0.3, df_name.x = "ae", df_name.y = "dm")
      )
    )
  )
  # Expect warning when a df_name doesn't match the only table of a variable
  expect_warning(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3, df_name.x = "ae", df_name.y = "ae")
      )
    ),
    regexp = "Data frame name ae will be ignored for correlation variable SEX as it only appears in one data frame.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect warning when a shared df_name doesn't match the table of a variable
  expect_warning(
    validate_correlations(
      multitable_marginals,
      list(
        correlation("SEX", "AESEV", 0.3, df_name = "ae")
      )
    ),
    regexp = "Data frame name ae will be ignored for correlation variable SEX as it only appears in one data frame.", #nolint: line_length_linter
    fixed = TRUE
  )
})
