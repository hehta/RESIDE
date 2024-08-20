test_that("generate_file_path works", {
  file_name <- "test.csv"
  folder_path <- tempdir()
  variable_type <- "continuous"
  expected_regex <- paste0(
    "Exporting ",
    variable_type,
    " to: ",
    "?.+"
  )
  file_path <- testthat::expect_output(
    generate_file_path(
      file_name,
      folder_path,
      variable_type
    ),
    expected_regex
  )
  testthat::expect_contains(
    file_path,
    file.path(normalizePath(folder_path), file_name)
  )
})
