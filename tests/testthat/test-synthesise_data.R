testthat::test_that("synthesise_data works", {
  marginals <- get_marginal_distributions(IST, c(
    variables = c(
      "SEX",
      "AGE",
      "ID14",
      "RSBP",
      "RATRIAL",
      "SET14D",
      "DSIDED"
    )
  ))
  sim_data <- synthesise_data(marginals)
  testthat::expect_true(is.data.frame(sim_data))
  testthat::expect_equal(marginals$summary$n_row, nrow(sim_data))
  testthat::expect_equal(marginals$summary$n_col + 1, ncol(sim_data))
  variables <- strsplit(marginals$summary$variables, ", ")[[1]]
  testthat::expect_true(all(
    variables %in% names(sim_data)
  ))
})

testthat::test_that("synthesise_data works", {
  testthat::expect_error(
    synthesise_data(list()),
    regexp = "^.*object must be of class RESIDE.*$"
  )
  marginals <- get_marginal_distributions(IST, c(
    variables = c(
      "SEX",
      "AGE",
      "ID14",
      "RSBP",
      "RATRIAL",
      "SET14D",
      "DSIDED"
    )
  ))
  temp_dir <- get_full_file_path(new_temp_dir(), "test")
  export_empty_cor_matrix(marginals, temp_dir)
  correlations <- import_cor_matrix(
    file.path(temp_dir, "correlation_matrix.csv")
  )
  sim_data_cor <- synthesise_data(marginals, correlations)
  testthat::expect_true(is.data.frame(sim_data_cor))
  testthat::expect_equal(marginals$summary$n_row, nrow(sim_data_cor))
  testthat::expect_equal(marginals$summary$n_col + 1, ncol(sim_data_cor))
  variables <- strsplit(marginals$summary$variables, ", ")[[1]]
  testthat::expect_true(all(
    variables %in% names(sim_data_cor)
  ))
})

test_probs <- c(0.25, 0.65, 0.1)

testthat::test_that("replace_zero_rows works", {
  test_row <- data.frame(
    dummy_1 = c(0, 1, 0),
    dummy_2 = c(0, 1, 1),
    dummy_3 = c(0, 1, 0)
  )
  exp_row <- data.frame(
    dummy_1 = c(0),
    dummy_2 = c(1),
    dummy_3 = c(0)
  )
  testthat::expect_equal(
    replace_zero_rows(test_row, test_probs),
    exp_row,
    ignore_attr = TRUE
  )
})

testthat::test_that("replace_one_rows works", {
  test_row <- data.frame(
    dummy_1 = c(0, 1, 0),
    dummy_2 = c(1, 1, 1),
    dummy_3 = c(0, 1, 0)
  )
  exp_row <- data.frame(
    dummy_1 = c(0, 0, 0),
    dummy_2 = c(1, 1, 1),
    dummy_3 = c(0, 0, 0)
  )
  testthat::expect_equal(
    replace_one_rows(test_row, test_probs),
    exp_row,
    ignore_attr = TRUE
  )
})

testthat::test_that("check_probs works", {
  testthat::expect_equal(check_probs(test_probs), test_probs)
  test_probs_2 <- c(0, 0, 1)
  testthat::expect_false(
    any(duplicated(check_probs(test_probs_2)))
  )
  test_probs_3 <- c(0, 1, 1)
  testthat::expect_false(
    any(duplicated(check_probs(test_probs_3)))
  )
})

testthat::test_that("get_data_def works", {
  data_def <- get_data_def(
    marginal_distributions
  )
  data_def_cor <- get_data_def(
    marginal_distributions,
    TRUE
  )
  testthat::expect_true(
    is.data.frame(data_def)
  )
  testthat::expect_true(
    is.data.frame(data_def_cor)
  )
  testthat::expect_equal(
    data_def[["dist"]],
    c("categorical", "categorical", "binary", "binary", "normal", "normal")
  )
  testthat::expect_equal(
    data_def_cor[["dist"]],
    c("binary", "binary", "binary",
      "binary", "binary", "binary",
      "binary", "normal", "normal"
    )
  )
  data_var_names <- c(
    names(marginal_distributions$categorical_variables),
    names(marginal_distributions$binary_variables),
    names(marginal_distributions$continuous_variables)
  )
  testthat::expect_true(
    all(data_def[["varname"]] %in% data_var_names)
  )
  categorical_summary <- marginal_distributions$categorical_variables
  cat_names <- c()
  for (.column in names(categorical_summary)){
    for (.cat in names(categorical_summary[[.column]])) {
      # Get the dummy variable name for the category
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the dummy variable name to the category names
      cat_names <- c(cat_names, cat_name)
    }
  }
  data_var_cor_names <- c(
    cat_names,
    names(marginal_distributions$binary_variables),
    names(marginal_distributions$continuous_variables)
  )
  testthat::expect_true(
    all(data_def_cor[["varname"]] %in% data_var_cor_names)
  )
})

testthat::test_that("define_categorical works", {
  categorical_summary <- marginal_distributions$categorical_variables
  n_row <- marginal_distributions$summary$n_row
  data_def <- NULL
  catergorical_def <- define_categorical(
    categorical_summary,
    n_row,
    data_def
  )
  testthat::expect_true(
    all(catergorical_def[["varname"]] %in% names(categorical_summary))
  )
  testthat::expect_equal(
    catergorical_def[["dist"]],
    c("categorical", "categorical")
  )
})

testthat::test_that("define_categorical_binary works", {
  categorical_summary <- marginal_distributions$categorical_variables
  n_row <- marginal_distributions$summary$n_row
  data_def <- NULL
  categorical_binary_def <- define_categorical_binary(
    categorical_summary,
    n_row,
    data_def
  )
  cat_names <- c()
  for (.column in names(categorical_summary)){
    for (.cat in names(categorical_summary[[.column]])) {
      # Get the dummy variable name for the category
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the dummy variable name to the category names
      cat_names <- c(cat_names, cat_name)
    }
  }
  testthat::expect_true(
    all(categorical_binary_def[["varname"]] %in% cat_names)
  )
  testthat::expect_equal(
    categorical_binary_def[["dist"]],
    c("binary", "binary", "binary", "binary", "binary")
  )
})

testthat::test_that("define_binary works", {
  binary_summary <- marginal_distributions$binary_variables
  data_def <- NULL
  binary_def <- define_binary(
    binary_summary,
    data_def
  )
  testthat::expect_true(
    all(binary_def[["varname"]] %in% names(binary_summary))
  )
  testthat::expect_equal(
    binary_def[["dist"]],
    c("binary", "binary")
  )
})

testthat::test_that("define_continuous works", {
  continuous_summary <- marginal_distributions$continuous_variables
  data_def <- NULL
  continuous_def <- define_continuous(
    continuous_summary,
    data_def
  )
  testthat::expect_true(
    all(continuous_def[["varname"]] %in% names(continuous_summary))
  )
  testthat::expect_equal(
    continuous_def[["dist"]],
    c("normal", "normal")
  )
})

testthat::test_that("export_empty_cor_matrix works", {
  # Test empty folder path
  testthat::expect_error(
    export_empty_cor_matrix(marginal_distributions),
    regexp = "A folder path must be provided."
  )
  # Test class assumption
  testthat::expect_error(
    export_empty_cor_matrix(list(), tempdir()),
    regexp = "^.*object must be of class RESIDE.*$"
  )
  marginals <- marginal_distributions
  temp_dir <- get_full_file_path(new_temp_dir(), "test")
  # Test folder doesn't exits
  testthat::expect_error(
    export_empty_cor_matrix(
      marginals,
      "./dirdoesnotexist",
      create_folder = FALSE
    ),
    regexp = "^.*Directory must exist, hint: set create_folder to TRUE.*$"
  )
  # Test folder creation
  export_empty_cor_matrix(
    marginals,
    temp_dir,
    "correlations.csv",
    create_folder = TRUE
  )
  testthat::expect_true(
    file.exists(
      normalizePath(
        file.path(temp_dir, "correlations.csv")
      )
    )
  )
})

testthat::test_that("import_cor_matrix_works", {
  # Test file exists
  expect_error(
    import_cor_matrix("./doesnotexist.csv"),
    regexp = "^.*Correlation file must exist.*$"
  )
  # Test Asymetric
  cor_asymetric <-
    testthat::test_path("testdata", "correlation_matrix_asymetric.csv")
  testthat::expect_error(
    import_cor_matrix(cor_asymetric),
    regexp = "^.*The correlation matrix needs to be symmetrical.*$"
  )
  # Test Non Finite
  cor_non_finite <-
    testthat::test_path("testdata", "correlation_matrix_non_finite.csv")
  testthat::expect_error(
    import_cor_matrix(cor_non_finite),
    regexp = "^.*The correlation matrix needs to be positive semi definite.*$"
  )
  marginals <- marginal_distributions
  temp_dir <- get_full_file_path(new_temp_dir(), "test")
  export_empty_cor_matrix(
    marginals,
    temp_dir,
    "correlations.csv",
    create_folder = TRUE
  )
  cor_matrix <- import_cor_matrix(
    file.path(temp_dir, "correlations.csv")
  )
  testthat::expect_true(
    is.matrix(cor_matrix)
  )
})