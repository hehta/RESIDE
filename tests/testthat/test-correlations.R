testthat::test_that("correlation works", {
  # Expect a correlation object with x, y and rho
  cor <- correlation("SEX", "AGE", 0.3)
  expect_s3_class(cor, "correlation")
  expect_equal(
    unclass(cor),
    list(x = "SEX", y = "AGE", rho = 0.3)
  )
  # Expect negative correlations and the boundaries to be valid
  expect_equal(correlation("SEX", "AGE", -0.3)$rho, -0.3)
  expect_equal(correlation("SEX", "AGE", -1)$rho, -1)
  expect_equal(correlation("SEX", "AGE", 1)$rho, 1)
  expect_equal(correlation("SEX", "AGE", 0)$rho, 0)
  # Expect the factor names to be added
  cor <- correlation(
    "SEX",
    "RATRIAL",
    0.3,
    factor_name.x = "M",
    factor_name.y = "Y"
  )
  expect_equal(cor$factor_name.x, "M")
  expect_equal(cor$factor_name.y, "Y")
  # Expect a single factor name to be added on its own
  cor <- correlation("SEX", "AGE", 0.3, factor_name.x = "M")
  expect_equal(cor$factor_name.x, "M")
  expect_false("factor_name.y" %in% names(cor))
  # Expect unknown parameters to be ignored
  cor <- correlation("SEX", "AGE", 0.3, unknown = "value")
  expect_equal(names(cor), c("x", "y", "rho"))
  # Expect error when rho is out of range
  expect_error(
    correlation("SEX", "AGE", 1.1),
    regexp = "Correlation coefficient (rho) must be a numeric value between -1 and 1.", #nolint: line_length_linter
    fixed = TRUE
  )
  expect_error(
    correlation("SEX", "AGE", -1.1),
    regexp = "Correlation coefficient (rho) must be a numeric value between -1 and 1.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when rho is not numeric
  expect_error(
    correlation("SEX", "AGE", "0.3"),
    regexp = "Correlation coefficient (rho) must be a numeric value between -1 and 1.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when x or y are not character strings
  expect_error(
    correlation(1, "AGE", 0.3),
    regexp = "Variable names (x and y) must be character strings.",
    fixed = TRUE
  )
  expect_error(
    correlation("SEX", 1, 0.3),
    regexp = "Variable names (x and y) must be character strings.",
    fixed = TRUE
  )
})

testthat::test_that("correlation works with multiple tables", {
  # Expect a shared df_name to be added
  cor <- correlation("AESEV", "AESER", 0.3, df_name = "ae")
  expect_equal(cor$df_name, "ae")
  expect_false(any(c("df_name.x", "df_name.y") %in% names(cor)))
  # Expect variable specific df_names to be added
  cor <- correlation(
    "SEX",
    "AESEV",
    0.3,
    df_name.x = "dm",
    df_name.y = "ae"
  )
  expect_equal(cor$df_name.x, "dm")
  expect_equal(cor$df_name.y, "ae")
  expect_false("df_name" %in% names(cor))
  # Expect df_names and factor names together
  cor <- correlation(
    "SEX",
    "AESEV",
    0.3,
    df_name.x = "dm",
    df_name.y = "ae",
    factor_name.x = "M",
    factor_name.y = "SEVERE"
  )
  expect_s3_class(cor, "correlation")
  expect_equal(
    unclass(cor),
    list(
      x = "SEX",
      y = "AESEV",
      rho = 0.3,
      df_name.x = "dm",
      df_name.y = "ae",
      factor_name.x = "M",
      factor_name.y = "SEVERE"
    )
  )
  # Expect error when only one of df_name.x and df_name.y is specified
  expect_error(
    correlation("SEX", "AESEV", 0.3, df_name.x = "dm"),
    regexp = "Both df_name.x and df_name.y must be provided if one is specified.", #nolint: line_length_linter
    fixed = TRUE
  )
  expect_error(
    correlation("SEX", "AESEV", 0.3, df_name.y = "ae"),
    regexp = "Both df_name.x and df_name.y must be provided if one is specified.", #nolint: line_length_linter
    fixed = TRUE
  )
})
