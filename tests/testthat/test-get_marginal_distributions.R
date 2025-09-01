testthat::test_that("Test get_marginal_distributions works as it should", {
  # Test non character variables
  testthat::expect_error(
    get_marginal_distributions(
      IST,
      variables = 1
    ),
    regexp = "^.*Variables must be a vector of characters.*$"
  )
  # Test missing variables
  testthat::expect_error(
    get_marginal_distributions(
      IST,
      variables = "notavariable"
    ),
    regexp = "^.*all variables must be in df missing: notavariable.*$"
  )
  # Test print
  testthat::expect_output(
    get_marginal_distributions(
      IST,
      print = TRUE
    ),
    regexp = "^.*Summary:.*Number of Rows:.*19435.*Number of Columns:.*112.+$"
  )
  # Test other data types
  testthat::expect_error(
    get_marginal_distributions(data.frame(test = 1, other = TRUE)),
    regexp = "^.*Unknown Variable type for column other$"
  )
  # Test unnamed list
  #@todo look at warning messages
  marginals_list_unnamed <- suppressWarnings(get_marginal_distributions(
    list(
      pharmaversesdtm::dm,
      pharmaversesdtm::cm,
      pharmaversesdtm::ae
    ),
    subject_identifier = "USUBJID"
  ))
  testthat::expect_true(
    all(
      "n_row.df.1" %in% names(marginals_list_unnamed$summary),
      "n_row.df.2" %in% names(marginals_list_unnamed$summary),
      "n_row.df.3" %in% names(marginals_list_unnamed$summary),
      "variables.df.1" %in% names(marginals_list_unnamed$summary),
      "variables.df.2" %in% names(marginals_list_unnamed$summary),
      "variables.df.3" %in% names(marginals_list_unnamed$summary)
    )
  )
  # Test named list
  testthat::expect_true(
    all(
      "n_row.df.dm" %in% names(longitudinal_marginals$summary),
      "n_row.df.cm" %in% names(longitudinal_marginals$summary),
      "n_row.df.ae" %in% names(longitudinal_marginals$summary),
      "variables.df.dm" %in% names(longitudinal_marginals$summary),
      "variables.df.cm" %in% names(longitudinal_marginals$summary),
      "variables.df.ae" %in% names(longitudinal_marginals$summary)
    )
  )

  # Test non long list
  marginal_list_non_long <- get_marginal_distributions(
    list(
      ist_1,
      ist_2
    ),
    subject_identifier = "id"
  )
  testthat::expect_true(
    all(
      "n_row.df.1" %in% names(marginal_list_non_long$summary),
      "n_row.df.2" %in% names(marginal_list_non_long$summary),
      "variables.df.1" %in% names(marginal_list_non_long$summary),
      "variables.df.2" %in% names(marginal_list_non_long$summary)
    )
  )
})

testthat::test_that(".prepare_df works", {
  # Test non character variables
  testthat::expect_error(
    .prepare_df(
      IST,
      variables = 1
    ),
    regexp = "^.*Variables must be a vector of characters.*$"
  )
  # Test missing variables
  testthat::expect_error(
    .prepare_df(
      IST,
      variables = "notavariable"
    ),
    regexp = "^.*all variables must be in df missing: notavariable.*$"
  )
  # test subject identifier not character
  testthat::expect_error(
    .prepare_df(
      IST,
      subject_identifier = TRUE
    ),
    regexp = "^.*Subject identifier must be a character.*$"
  )
})

testthat::test_that(".remove_subject_identifier works", {
  # Test subject identifier not character
  testthat::expect_error(
    .remove_subject_identifier(
      IST,
      subject_identifier = TRUE
    ),
    regexp = "^.*Subject identifier must be a character.*$"
  )
  test_df <- .remove_subject_identifier(
    pharmaversesdtm::dm,
    subject_identifier = "USUBJID"
  )
  testthat::expect_false(
    "USUBJID" %in% names(test_df)
  )
})