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

testthat::test_that(".write_csv works", {
  file_name <- "test.csv"
  folder_path <- new_temp_dir()
  file_path <- get_full_file_path(
    folder_path,
    "test.csv"
  )
  variable_type <- "continuous"
  expected_regex <- paste0(
    "Exporting?.+",
    variable_type,
    " to: ?.+",
    file_name
  )
  df <- data.frame(test = "test")
  testthat::expect_message(
    .write_csv(
      df,
      file_path,
      variable_type
    ),
    expected_regex
  )
  testthat::expect_true(file.exists(file_path))
})

testthat::test_that("marginal_files_exist", {
  expected_files <- c(
    "binary_variables.csv",
    "categorical_variables.csv",
    "continuous_variables.csv",
    "continuous_quantiles.csv",
    "summary.csv"
  )
  temp_dir <- new_temp_dir()
  export_marginal_distributions(
    marginal_distributions,
    temp_dir
  )
  expect_equal(
    marginal_files_exist(temp_dir),
    expected_files
  )
})

testthat::test_that("remove_marginal_files works", {
  temp_dir <- new_temp_dir()
  export_marginal_distributions(
    marginal_distributions,
    temp_dir
  )
  remove_marginal_files(temp_dir)
  expect_equal(
    marginal_files_exist(temp_dir),
    c()
  )
  export_marginal_distributions(
    marginal_distributions,
    temp_dir
  )
  testthat::local_mocked_bindings(
    marginal_files_exist = function(...) c("other_file.csv")
  )
  testthat::expect_error(
    remove_marginal_files(temp_dir),
    regexp = "^.*other_file.csv.*$"
  )
  expect_equal(
    marginal_files_exist(temp_dir),
    c("other_file.csv")
  )
})