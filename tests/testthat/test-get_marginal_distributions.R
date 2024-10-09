testthat::test_that("Test get_marginal_distributions works as it should", {
  # Test non character variables
  testthat::expect_error(
    get_marginal_distributions(
      IST,
      variables = 1
    ),
    regexp = "^.*Variables must be a vector of characters.*$"
  )
  # Test missing variables
  testthat::expect_error(
    get_marginal_distributions(
      IST,
      variables = "notavariable"
    ),
    regexp = "^.*all variables must be in df missing: notavariable.*$"
  )
  # Test print
  testthat::expect_output(
    get_marginal_distributions(
      IST,
      print = TRUE
    ),
    regexp = "^.*Summary:.*Number of Rows:.*19435.*Number of Columns:.*112.+$"
  )
  # Test other data types
  testthat::expect_error(
    get_marginal_distributions(data.frame(test = 1, other = TRUE)),
    regexp = "^.*Unknown Variable type for column other$"
  )
})