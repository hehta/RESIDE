testthat::test_that("export_empty_cor_matrix is defunct", {
  testthat::expect_error(
    export_empty_cor_matrix(),
    class = "defunctError",
    regexp = "export_empty_cor_matrix() has been removed",
    fixed = TRUE
  )
  # Legacy arguments are ignored, the function still errors
  testthat::expect_error(
    export_empty_cor_matrix(list(), tempdir()),
    class = "defunctError",
    regexp = "Use the correlation() function",
    fixed = TRUE
  )
})

testthat::test_that("import_cor_matrix is defunct", {
  testthat::expect_error(
    import_cor_matrix(),
    class = "defunctError",
    regexp = "import_cor_matrix() has been removed",
    fixed = TRUE
  )
  # Legacy arguments are ignored, the function still errors
  testthat::expect_error(
    import_cor_matrix("correlation_matrix.csv"),
    class = "defunctError",
    regexp = "Use the correlation() function",
    fixed = TRUE
  )
})

testthat::test_that("synthesise_data works", {
  set.seed(1234)
  sim_df <- synthesise_data(marginal_distributions)
  sub_marginals <- marginal_distributions[[1]]
  # Expect a data frame with a row for each row in the marginals
  expect_true(is.data.frame(sim_df))
  expect_equal(nrow(sim_df), sub_marginals$summary$n_row)
  # Expect an id column and a column for each variable
  expect_equal(
    names(sim_df),
    c("id", get_submarginal_variables(sub_marginals))
  )
  # Expect error when not class RESIDE
  expect_error(
    synthesise_data(list()),
    regexp = "object must be of class RESIDE",
    fixed = TRUE
  )
})

testthat::test_that("synthesise_data works with correlations", {
  set.seed(1234)
  sub_marginals <- marginal_distributions[[1]]
  sim_df <- synthesise_data(
    marginal_distributions,
    correlations = list(
      correlation("AGE", "RSBP", 0.6),
      correlation("SEX", "AGE", -0.5, factor_name.x = "M")
    )
  )
  # Expect the same structure as without correlations
  expect_true(is.data.frame(sim_df))
  expect_equal(nrow(sim_df), sub_marginals$summary$n_row)
  expect_equal(
    names(sim_df),
    c("id", get_submarginal_variables(sub_marginals))
  )
  expect_equal(sim_df$id, seq_len(sub_marginals$summary$n_row))
  # Expect the categories to be those of the marginals
  expect_true(
    all(sim_df$SEX %in% names(sub_marginals$categorical_variables$SEX))
  )
  # Expect the categorical marginals to be maintained
  expect_equal(
    mean(sim_df$SEX == "M"),
    unname(
      sub_marginals$categorical_variables$SEX[["M"]] /
        sub_marginals$summary$n_row
    ),
    tolerance = 0.05
  )
  # Expect continuous variables within the range of the quantiles
  age_quantiles <- sub_marginals$continuous_variables$AGE$quantiles$orig_q
  expect_true(
    all(
      sim_df$AGE >= min(age_quantiles) & sim_df$AGE <= max(age_quantiles),
      na.rm = TRUE
    )
  )
  # Expect the correlations to be in the specified direction
  expect_gt(
    cor(sim_df$AGE, sim_df$RSBP, method = "spearman", use = "complete.obs"),
    0.3
  )
  expect_lt(
    cor(sim_df$SEX == "M", sim_df$AGE, use = "complete.obs"),
    -0.1
  )
  # Expect a single correlation to be accepted
  expect_no_error(
    synthesise_data(
      marginal_distributions,
      correlations = correlation("AGE", "RSBP", 0.5)
    )
  )
})

testthat::test_that("synthesise_data errors with invalid correlations", {
  # Expect error when a categorical variable has no factor name
  expect_error(
    synthesise_data(
      marginal_distributions,
      correlations = list(correlation("SEX", "AGE", 0.3))
    ),
    regexp = "Correlation variable SEX is categorical and must have a factor name specified using factor_name.x", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when the correlations are not correlation objects
  expect_error(
    synthesise_data(
      marginal_distributions,
      correlations = list(list(x = "AGE", y = "RSBP", rho = 0.3))
    ),
    regexp = "correlations must be a list of correlations created with the correlation() function.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when the correlations are inconsistent
  expect_error(
    synthesise_data(
      marginal_distributions,
      correlations = list(
        correlation("AGE", "RSBP", 0.9),
        correlation("AGE", "SET14D", 0.9),
        correlation("RSBP", "SET14D", -0.9)
      )
    ),
    regexp = "The correlations are not consistent with each other, the correlation matrix is not positive semi definite.", #nolint: line_length_linter
    fixed = TRUE
  )
  # Expect error when a correlation matrix is supplied
  expect_error(
    synthesise_data(marginal_distributions, correlation_matrix = diag(2)),
    regexp = "correlation_matrix is no longer supported",
    fixed = TRUE
  )
  # Expect validation errors
  expect_error(
    synthesise_data(
      marginal_distributions,
      correlations = list(correlation("NOTAVARIABLE", "AGE", 0.3))
    ),
    regexp = "The following variables are not present in the marginals: NOTAVARIABLE", #nolint: line_length_linter
    fixed = TRUE
  )
})

testthat::test_that("synthesise_data works with correlations and multiple tables", { #nolint: line_length_linter
  set.seed(1234)
  sim_dfs <- synthesise_data(
    multitable_marginals,
    correlations = list(
      correlation("AGE", "SEX", -0.5, factor_name.y = "M"),
      correlation(
        "SEX",
        "AESEV",
        0.8,
        df_name.x = "dm",
        df_name.y = "ae",
        factor_name.x = "M",
        factor_name.y = "MODERATE"
      ),
      correlation("DOMAIN", "AESTDY", 0.2, df_name = "ae", factor_name.x = "AE")
    )
  )
  # Expect a data frame for each table with the same structure
  # as without correlations
  expect_equal(names(sim_dfs), c("dm", "cm", "ae"))
  for (df_name in names(sim_dfs)) {
    sub_marginals <- multitable_marginals[[df_name]]
    expect_equal(nrow(sim_dfs[[df_name]]), sub_marginals$summary$n_row)
    expect_equal(
      names(sim_dfs[[df_name]]),
      c("USUBJID", get_submarginal_variables(sub_marginals))
    )
    # Expect the number of subjects of each table
    expect_equal(
      length(unique(sim_dfs[[df_name]]$USUBJID)),
      sub_marginals$summary$n_subjects,
      tolerance = 0.1
    )
  }
  # Expect subjects to be shared across tables
  expect_true(
    all(sim_dfs$ae$USUBJID %in% seq_len(
      multitable_marginals$overall_summary$n_subjects
    ))
  )
  # Expect correlated variables to take a single value per subject
  expect_true(
    all(tapply(sim_dfs$ae$AESTDY, sim_dfs$ae$USUBJID, function(x) {
      length(unique(x[!is.na(x)])) <= 1
    }))
  )
  # Expect the correlation within a table
  expect_lt(
    cor(sim_dfs$dm$SEX == "M", sim_dfs$dm$AGE, use = "complete.obs"),
    -0.1
  )
  # Expect the correlation across tables, by subject
  joined <- merge(
    sim_dfs$dm[, c("USUBJID", "SEX")],
    sim_dfs$ae[, c("USUBJID", "AESEV")],
    by = "USUBJID"
  )
  expect_gt(
    cor(joined$SEX == "M", joined$AESEV == "MODERATE"),
    0.1
  )
  # Expect the common variables to be synthesised
  for (df_name in names(sim_dfs)) {
    expect_true(all(sim_dfs[[df_name]]$STUDYID == "CDISCPILOT01"))
  }
})

testthat::test_that("synthesise_data works with correlated common variables", { #nolint: line_length_linter
  set.seed(1234)
  sim_dfs <- synthesise_data(
    multitable_marginals,
    correlations = list(
      correlation("STUDYID", "AGE", 0.1, factor_name.x = "CDISCPILOT01")
    )
  )
  # Expect the common variable in each table
  for (df_name in names(sim_dfs)) {
    expect_true(all(sim_dfs[[df_name]]$STUDYID == "CDISCPILOT01"))
  }
  # Expect error when a duplicated variable has no df_name
  expect_error(
    synthesise_data(
      multitable_marginals,
      correlations = list(
        correlation("DOMAIN", "AGE", 0.3, factor_name.x = "AE")
      )
    ),
    regexp = "Correlation variable DOMAIN is duplicated in the marginals and must have a data frame name specified.", #nolint: line_length_linter
    fixed = TRUE
  )
})

testthat::test_that("generate_correlation_matrix works", {
  sub_marginals <- marginal_distributions[[1]]
  cor_matrix <- generate_correlation_matrix(
    sub_marginals,
    list(
      correlation("AGE", "RSBP", 0.5),
      correlation("SEX", "AGE", -0.3, factor_name.x = "M")
    )
  )
  # Expect a symmetric matrix with a row for each (dummy) variable
  expect_true(isSymmetric(cor_matrix))
  expect_equal(
    rownames(cor_matrix),
    get_data_def(sub_marginals, TRUE)[["varname"]]
  )
  expect_equal(unname(diag(cor_matrix)), rep(1, nrow(cor_matrix)))
  # Expect the correlations to be set
  expect_equal(cor_matrix["AGE", "RSBP"], 0.5)
  expect_equal(cor_matrix["RSBP", "AGE"], 0.5)
  # Expect the categorical correlation to use the dummy variable
  expect_equal(cor_matrix["SEX_M", "AGE"], -0.3)
  expect_equal(cor_matrix["SEX_F", "AGE"], 0)
  # Expect error when a categorical variable has no factor name
  expect_error(
    generate_correlation_matrix(
      sub_marginals,
      list(correlation("SEX", "AGE", 0.3))
    ),
    regexp = "Correlation variable SEX is categorical and must have a factor name specified using factor_name.x", #nolint: line_length_linter
    fixed = TRUE
  )
})

testthat::test_that("get_correlated_categories works", {
  expect_equal(
    get_correlated_categories(
      list(
        correlation("SEX", "AGE", 0.3, factor_name.x = "M"),
        correlation("AGE", "RATRIAL", 0.3, factor_name.y = "Y"),
        correlation("SEX", "RSBP", 0.3, factor_name.x = "F"),
        correlation("AGE", "RSBP", 0.3)
      )
    ),
    list(SEX = c("M", "F"), RATRIAL = "Y")
  )
  expect_equal(get_correlated_categories(list()), list())
})

testthat::test_that("replace_zero_rows and replace_one_rows work", {
  set.seed(1234)
  rows <- data.frame(a = c(0, 0, 0), b = c(0, 0, 0), c = c(0, 0, 0))
  # Expect a single category to be selected for each row
  replaced <- replace_zero_rows(rows, c(0.2, 0.3, 0.5))
  expect_equal(names(replaced), names(rows))
  expect_equal(unname(rowSums(replaced)), c(1, 1, 1))
  # Expect categories with a probability of 0 to not be selected
  replaced <- replace_zero_rows(rows, c(0, 0, 1))
  expect_equal(replaced$c, c(1, 1, 1))
  # Expect one of the selected categories to be chosen
  rows <- data.frame(a = c(1, 1, 0), b = c(1, 0, 1), c = c(0, 1, 1))
  replaced <- replace_one_rows(rows, c(0.2, 0.3, 0.5))
  expect_equal(unname(rowSums(replaced)), c(1, 1, 1))
  expect_true(all(as.matrix(replaced) <= as.matrix(rows)))
})

testthat::test_that("fix_factors works", {
  set.seed(1234)
  sub_marginals <- list(
    categorical_variables = list(
      COLOUR = c(red = 50, green = 30, blue = 20)
    ),
    summary = data.frame(n_row = 100)
  )
  n <- 10000
  simulated_data <- data.frame(
    COLOUR_red = rbinom(n, 1, 0.5),
    COLOUR_green = rbinom(n, 1, 0.3),
    COLOUR_blue = rbinom(n, 1, 0.2)
  )
  # Expect a single category for each row
  fixed <- fix_factors(simulated_data, sub_marginals)
  expect_true(all(rowSums(fixed) == 1))
  # Expect the correlated category to be kept and the marginals maintained
  fixed <- fix_factors(
    simulated_data,
    sub_marginals,
    list(COLOUR = "blue")
  )
  expect_true(all(rowSums(fixed) == 1))
  expect_equal(fixed$COLOUR_blue, simulated_data$COLOUR_blue)
  expect_equal(mean(fixed$COLOUR_red), 0.5, tolerance = 0.05)
  expect_equal(mean(fixed$COLOUR_green), 0.3, tolerance = 0.05)
})

testthat::test_that("restore_factors works", {
  simulated_data <- data.frame(
    COLOUR_red = c(1, 0, 0),
    COLOUR_green = c(0, 1, 1),
    COLOUR_blue = c(0, 0, 0)
  )
  # Expect the categories to be restored, including unselected categories
  restored <- restore_factors(
    simulated_data,
    list(COLOUR = c(red = 1, green = 2, blue = 0))
  )
  expect_equal(names(restored), "COLOUR")
  expect_equal(restored$COLOUR, c("red", "green", "green"))
})
