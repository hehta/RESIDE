testthat::test_that("export_marginal_distributions works", {
  # Check folder path
  testthat::expect_error(
    export_marginal_distributions(list()),
    regexp = "^.*A folder path must be provided.*$"
  )
  # Test class assumption
  testthat::expect_error(
    export_marginal_distributions(list(), tempdir()),
    regexp = "^.*object must be of class RESIDE.*$"
  )
  marginals <- marginal_distributions
  temp_dir <- get_full_file_path(new_temp_dir(), "test")
  # Test folder doesn't exits
  testthat::expect_error(
    export_marginal_distributions(
      marginals,
      "./dirdoesnotexist"
    ),
    regexp = "^.*Directory must exist, hint: set create_folder to TRUE.*$"
  )
  # Test folder creation
  export_marginal_distributions(
    marginals,
    temp_dir,
    create_folder = TRUE
  )
  testthat::expect_true(
    all(
      dir.exists(temp_dir),
      file.exists(
        normalizePath(
          file.path(temp_dir, "categorical_variables.csv")
        )
      ),
      file.exists(
        normalizePath(
          file.path(temp_dir, "binary_variables.csv")
        )
      ),
      file.exists(
        normalizePath(
          file.path(temp_dir, "continuous_variables.csv")
        )
      ),
      file.exists(
        normalizePath(
          file.path(temp_dir, "continuous_quantiles.csv")
        )
      ),
      file.exists(
        normalizePath(
          file.path(temp_dir, "summary.csv")
        )
      )
    )
  )
})

testthat::test_that("export_marginal_distributions errors", {
  temp_dir <- new_temp_dir()
  export_marginal_distributions(
    marginal_distributions,
    temp_dir
  )
  testthat::expect_error(
    export_marginal_distributions(
      marginal_distributions,
      temp_dir
    ),
    regexp = "^Marginal files:.*already exists.*$"
  )
})