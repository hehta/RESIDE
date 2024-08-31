test_that("Import Marginal Distributions works", {
  temp_dir <- tempdir()
  expected_marginals <- get_marginal_distributions(
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