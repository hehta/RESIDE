testthat::test_that("generate_file_path works", {
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

testthat::test_that("get_full_file_path works", {
  file_path <- get_full_file_path(
    ".",
    "test.csv"
  )
  testthat::expect_type(file_path, "character")
  testthat::expect_true(grepl("test.csv$", file_path))
})


testthat::test_that("get_variables_path works", {
  testthat::expect_message(
    get_variables_path(".", "", "binary"),
    regexp = "Info: No file for binary variables found"
  )
  testthat::expect_equal(
    "",
    get_variables_path(".", "", "binary")
  )
  testthat::expect_error(
    get_variables_path(".", "nonexistent.csv", "binary"),
    regexp = "binary variables file .+ must Exist"
  )
})

testthat::test_that("load_variables_file works", {
  expect_true(
    is.data.frame(
      load_variables_file("", "binary")
    )
  )
  testthat::expect_error(
    load_variables_file("nonexistent.csv", "binary"),
    regexp = "Error reading binary variables from nonexistent.csv does the file exist?" # nolint line_length
  )
})
