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
  testthat::expect_equal(
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

testthat::test_that("is_long_format works", {
  testthat::expect_false(
    is_long_format(pharmaversesdtm::dm, "USUBJID")
  )
  testthat::expect_true(
    is_long_format(pharmaversesdtm::ae, "USUBJID")
  )
  testthat::expect_error(
    is_long_format(pharmaversesdtm::dm, "nonexistent"),
  )
})

testthat::test_that("get_long_columns works", {
  expected <- c(
    "AESER",
    "AESDISAB",
    "AESCAN",
    "AESLIFE",
    "AESDTH",
    "AESHOSP",
    "AEDTC",
    "AESEV",
    "AEENDY",
    "AESTDY",
    "AEENDTC",
    "AESTDTC",
    "AEOUT",
    "AEREL",
    "AESOC",
    "AEBODSYS",
    "AEHLGT",
    "AEHLT",
    "AEDECOD",
    "AELLT",
    "AETERM",
    "AESPID",
    "AESEQ"
  )
  testthat::expect_equal(
    expected,
    get_long_columns(pharmaversesdtm::ae, "USUBJID")
  )
})

testthat::test_that("long_to_wide works", {
  subject_identifier <- "USUBJID"
  unique_subjects <- unique(pharmaversesdtm::ae[[subject_identifier]])
  testthat::expect_warning(
    testthat::expect_equal(
      nrow(long_to_wide(pharmaversesdtm::ae, subject_identifier)),
      length(unique_subjects)
    )
  )
})

testthat::test_that("is_multi_table works", {
  testthat::expect_false(
    is_multi_table(marginal_distributions)
  )
  testthat::expect_true(
    is_multi_table(longitudinal_marginals)
  )
})

testthat::test_that("is_multi_table_long works", {
  testthat::expect_false(
    is_multi_table_long(marginal_distributions)
  )
  testthat::expect_true(
    is_multi_table_long(longitudinal_marginals)
  )
})

testthat::test_that(".filter_marginals_works", {
  variables <- c("AGEU", "SEX", "RACE")
  filtered_marginals <- .filter_marginals(
    longitudinal_marginals,
    variables
  )
  testthat::expect_equal(
    filtered_marginals$summary$variables,
    paste0(variables, collapse = ", ")
  )
  testthat::expect_equal(
    filtered_marginals$summary$n_col,
    length(variables)
  )
  variables <- c("SEX", "AGE", "ID14")
  filtered_marginals <- .filter_marginals(
    marginal_distributions,
    variables
  )
  testthat::expect_equal(
    filtered_marginals$summary$variables,
    paste0(variables, collapse = ", ")
  )
  testthat::expect_equal(
    filtered_marginals$summary$n_col,
    length(variables)
  )
})

testthat::test_that("get_ncol works", {
  n_col <- get_n_col(marginal_distributions)
  testthat::expect_equal(
    n_col,
    marginal_distributions$summary$n_col
  )
})

testthat::test_that("get_variables works", {
  variables <- get_variables(marginal_distributions)
  testthat::expect_setequal(
    variables,
    .split_variables(marginal_distributions$summary$variables)
  )
})

testthat::test_that("get_summary_variables works", {
  summary_variables <- get_summary_variables(marginal_distributions)
  testthat::expect_setequal(
    summary_variables,
    .split_variables(marginal_distributions$summary$variables)
  )
  no_summary_marginals <- marginal_distributions
  no_summary_marginals$summary$variables <- NULL
  testthat::expect_error(
    get_summary_variables(no_summary_marginals),
    regexp = "^Marginals summary does not contain variables\\.$"
  )
  no_summary_marginals$summary <- NULL
  testthat::expect_error(
    get_summary_variables(no_summary_marginals),
    regexp = "^Marginals do not contain a summary\\.$"
  )
})

testthat::test_that(".get_keys works", {
  testthat::expect_setequal(
    .get_keys(longitudinal_marginals),
    names(dfs)
  )
})