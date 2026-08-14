testthat::test_that("get_missing_variables works", {
  testthat::expect_equal(
    length(get_missing_variables(list(ist_id), "AGE")),
    0
  )
  testthat::expect_equal(
    length(get_missing_variables(list(ist_id), "test")),
    1
  )
  testthat::expect_equal(
    get_missing_variables(list(ist_id), "test"),
    c("test")
  )
  testthat::expect_equal(
    get_missing_variables(dfs, c("nonexistant", "test")),
    c("nonexistant", "test")
  )
  testthat::expect_equal(
    length(get_missing_variables(dfs, c("DOMAIN", "RACE", "AEDTC"))),
    0
  )
  testthat::expect_equal(
    length(get_missing_variables(dfs, "test")),
    1
  )
})

testthat::test_that("filter_variables works", {
  testthat::expect_equal(
    ncol(filter_variables(list(ist_id), c("AGE", "SEX"))[[1]]),
    2
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
  test_path <- testthat::test_path("testdata", "binary_variables.csv")
  testthat::expect_silent(
    get_variables_path(
      dirname(test_path),
      basename(test_path),
      "binary"
    )
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
  expect_true(
    is.data.frame(
      load_variables_file(
        testthat::test_path("testdata", "binary_variables.csv"),
        "binary"
      )
    )
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

testthat::test_that(".filter_sub_marginals_works", {
  variables <- c("SEX", "AGE", "ID14")
  filtered_marginals <- .filter_sub_marginals(
    marginal_distributions[[1]],
    variables
  )
  testthat::expect_true(
    all(variables %in%
        .split_variables(filtered_marginals$summary$variables)
    )
  )
  testthat::expect_true(
    length(
      .split_variables(filtered_marginals$summary$variables)
    ) == length(variables)
  )
  testthat::expect_equal(
    filtered_marginals$summary$n_col,
    length(variables)
  )
  testthat::expect_equal(
    filtered_marginals$summary$n_row,
    nrow(IST)
  )
  filtered_marginals_2 <- .filter_sub_marginals(
    multitable_marginals[["dm"]],
    c("SEX", "AGE")
  )
  testthat::expect_equal(
    filtered_marginals_2$summary$variables,
    paste0(c("SEX", "AGE"), collapse = ", ")
  )
  testthat::expect_equal(
    filtered_marginals_2$summary$n_col,
    2
  )
  testthat::expect_equal(
    filtered_marginals_2$summary$n_row,
    nrow(pharmaversesdtm::dm)
  )
  testthat::expect_equal(
    filtered_marginals_2$summary$subject_identifier,
    "USUBJID"
  )
  filtered_marginals_3 <- .filter_sub_marginals(
    multitable_marginals[["ae"]],
    c("AEDECOD", "AESTDTC"),
    keep_subject_identifier = FALSE
  )
  testthat::expect_equal(
    filtered_marginals_3$summary$variables,
    paste0(c("AEDECOD", "AESTDTC"), collapse = ", ")
  )
  testthat::expect_equal(
    filtered_marginals_3$summary$n_col,
    2
  )
  testthat::expect_equal(
    filtered_marginals_3$summary$n_row,
    nrow(pharmaversesdtm::ae)
  )
  testthat::expect_equal(
    filtered_marginals_3$summary$subject_identifier,
    ""
  )
})

testthat::test_that(".filter_marginals works", {
  filter_variables <- c("SEX", "AGEU")
  filtered_marginals <- .filter_marginals(
    multitable_marginals,
    filter_variables
  )
  print(filtered_marginals)
  testthat::expect_true(
    all(
      "ae" %in% names(filtered_marginals),
      "dm" %in% names(filtered_marginals),
      "cm" %in% names(filtered_marginals),
      "overall_summary" %in% names(filtered_marginals)
    )
  )
  testthat::expect_true(
    all(
      length(names(filtered_marginals[["dm"]])) == 2,
      length(names(filtered_marginals[["cm"]])) == 1,
      length(names(filtered_marginals[["ae"]])) == 1,
      length(names(filtered_marginals[["overall_summary"]])) == 5
    )
  )
  testthat::expect_true(
    all(
      filtered_marginals[["dm"]]$summary$subject_identifier == "USUBJID",
      filtered_marginals[["dm"]]$summary$n_row == 306,
      filtered_marginals[["dm"]]$summary$n_col == 2,
      filtered_marginals[["dm"]]$summary$data_frame == "dm"
    )
  )
  testthat::expect_true(
    all(filter_variables %in%
        .split_variables(filtered_marginals[["dm"]]$summary$variables)
    )
  )
  testthat::expect_equal(
    filtered_marginals$overall_summary$common_columns,
    ""
  )
  filter_variables_2 <- c("SEX", "AGEU", "STUDYID")
  filtered_marginals_2 <- .filter_marginals(
    multitable_marginals,
    filter_variables_2
  )
  testthat::expect_true(
    all(
      "ae" %in% names(filtered_marginals_2),
      "dm" %in% names(filtered_marginals_2),
      "cm" %in% names(filtered_marginals_2),
      "overall_summary" %in% names(filtered_marginals_2)
    )
  )
  testthat::expect_true(
    all(
      length(names(filtered_marginals_2[["dm"]])) == 2,
      length(names(filtered_marginals_2[["cm"]])) == 2,
      length(names(filtered_marginals_2[["ae"]])) == 2,
      length(names(filtered_marginals_2[["overall_summary"]])) == 5
    )
  )
  testthat::expect_equal(
    filtered_marginals_2$overall_summary$common_columns,
    "STUDYID"
  )
})

# testthat::test_that(".filter_common_marginals works", {
#   filtered_marginals <- .filter_common_marginals(
#     multitable_marginals
#   )
#   testthat::expect_true(
#     all(
#       "ae" %in% names(filtered_marginals),
#       "dm" %in% names(filtered_marginals),
#       "cm" %in% names(filtered_marginals),
#       "overall_summary" %in% names(filtered_marginals)
#     )
#   )
#   testthat::expect_true(
#     all(
#       length(names(filtered_marginals[["dm"]])) == 2,
#       length(names(filtered_marginals[["cm"]])) == 2,
#       length(names(filtered_marginals[["ae"]])) == 2,
#       length(names(filtered_marginals[["overall_summary"]])) == 5
#     )
#   )
#   testthat::expect_equal(
#     filtered_marginals$overall_summary$common_columns,
#     "STUDYID"
#   )
# })

testthat::test_that("get_variables works", {
  variables <- get_variables(marginal_distributions)
  testthat::expect_setequal(
    variables,
    c(
      "SEX",
      "AGE",
      "ID14",
      "RSBP",
      "RATRIAL",
      "SET14D"
    )
  )
})

testthat::test_that(".is_date works", {
  testthat::expect_true(
    .is_date(dfs$ae$AEDTC)
  )
  testthat::expect_true(
    .is_date(dfs$dm$RFPENDTC)
  )
  testthat::expect_false(
    .is_date(dfs$dm$STUDYID)
  )
  testthat::expect_false(
    .is_date(c("2020-01-01", "01", "02", "03", "05"))
  )
})

testthat::test_that(".get_common_fields works", {
  common_columns <- .get_common_fields(multitable_marginals)
  testthat::expect_setequal(
    common_columns,
    c("STUDYID")
  )
  testthat::expect_equal(
    .get_common_fields(marginal_distributions),
    as.character()
  )
})

testthat::test_that(".replace_nas works", {
  test_df <- data.frame(
    a = c("a", "b", "NA's", "d")
  )
  expected_df <- data.frame(
    a = c("a", "b", "", "d")
  )
  testthat::expect_equal(
    .replace_nas(test_df),
    expected_df
  )
})

testthat::test_that("get_all_types works", {
  testthat::expect_setequal(
    get_all_types(marginal_distributions),
    c("binary_variables", "categorical_variables", "continuous_variables")
  )
  testthat::expect_setequal(
    get_all_types(multitable_marginals),
    c("binary_variables", "categorical_variables", "continuous_variables")
  )
})

testthat::test_that("get_dates_from_sub_marginals works", {
  ae_dates <- get_dates_from_sub_marginals(multitable_marginals[["ae"]])
  testthat::expect_true(
    all(
      c("AEDTC", "AESTDTC", "AEENDTC") %in% ae_dates
    )
  )
  cm_dates <- get_dates_from_sub_marginals(multitable_marginals[["cm"]])
  testthat::expect_true(
    all(
      c("CMENDTC") %in% cm_dates
    )
  )
  dm_dates <- get_dates_from_sub_marginals(multitable_marginals[["dm"]])
  testthat::expect_true(
    all(
      c("RFSTDTC", "RFXSTDTC", "RFPENDTC", "DTHDTC", "DMDTC") %in% dm_dates
    )
  )
})