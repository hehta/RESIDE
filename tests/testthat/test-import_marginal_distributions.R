testthat::test_that("Import Marginal Distributions works", {
  temp_dir <- new_temp_dir()
  expected_marginals <- marginal_distributions
  export_marginal_distributions(
    expected_marginals,
    folder_path = temp_dir
  )
  imported_marginals <- import_marginal_distributions(
    folder_path = temp_dir
  )
  testthat::expect_equal(
    expected_marginals,
    imported_marginals
  )
})