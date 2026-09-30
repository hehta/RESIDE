testthat::test_that("Test get_marginal_distributions works as it should", {
  testthat::skip_if_not_installed("pharmaversesdtm")
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
    regexp = "^.*all variables must be in data missing: notavariable.*$",
    inherit = TRUE
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
      "" %in% names(marginals_list_unnamed),
      "overall_summary" %in% names(marginals_list_unnamed),
      marginals_list_unnamed$overall_summary$n_data_frames == 3,
      marginals_list_unnamed$overall_summary$data_frame_names == "1, 2, 3",
      marginals_list_unnamed$overall_summary$subject_identifier == "USUBJID",
      marginals_list_unnamed$overall_summary$common_columns == "STUDYID"
    )
  )
  # Test named list
  testthat::expect_true(
    all(
      "dm" %in% names(multitable_marginals),
      "cm" %in% names(multitable_marginals),
      "ae" %in% names(multitable_marginals),
      "overall_summary" %in% names(multitable_marginals),
      multitable_marginals$overall_summary$n_data_frames == 3,
      multitable_marginals$overall_summary$data_frame_names == "dm, cm, ae",
      multitable_marginals$overall_summary$subject_identifier == "USUBJID",
      multitable_marginals$overall_summary$common_columns == "STUDYID"
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
      "" %in% names(marginal_list_non_long),
      "overall_summary" %in% names(marginal_list_non_long),
      marginal_list_non_long$overall_summary$n_data_frames == 2,
      marginal_list_non_long$overall_summary$data_frame_names == "1, 2",
      marginal_list_non_long$overall_summary$subject_identifier == "id",
      marginal_list_non_long$overall_summary$common_columns == ""
    )
  )
})

testthat::test_that("is_subject_identifier works", {
  testthat::expect_true(
    is_subject_identifier(
      list(
        ist_1,
        ist_2
      ),
      subject_identifier = "id"
    )
  )
  ist_3 <- ist_2 %>% dplyr::select(-id)
  testthat::expect_false(
    is_subject_identifier(
      list(
        ist_1,
        ist_3
      ),
      subject_identifier = "id"
    )
  )
})

testthat::test_that(".prepare_dfs works", {
  # Test non character variables
  testthat::expect_error(
    .prepare_dfs(
      list(
        ist_1,
        ist_2
      ),
      variables = 1,
      subject_identifier = "id"
    ),
    regexp = "^.*Variables must be a vector of characters.*$"
  )
  # Test missing variables
  testthat::expect_error(
    .prepare_dfs(
      list(
        ist_1,
        ist_2
      ),
      variables = "notavariable",
      subject_identifier = "id"
    ),
    regexp = "^.*all variables must be in data missing: notavariable*$"
  )
  # test subject identifier not character
  testthat::expect_error(
    .prepare_dfs(
      list(
        ist_1,
        ist_2
      ),
      subject_identifier = TRUE
    ),
    regexp = "^.*Subject identifier must be a character.*$"
  )
  ist_3 <- ist_2 %>% dplyr::select(-id)
  # test subject identifier not present in all data frames
  testthat::expect_error(
    .prepare_dfs(
      list(
        ist_1,
        ist_3
      ),
      subject_identifier = "id",
      variables = c(),
      retype = TRUE
    ),
    regexp = "^.*Subject identifier id must be present in all data frames.*$"
  )
  prepared_dfs <- .prepare_dfs(
    list(
      ist_1,
      ist_2
    ),
    subject_identifier = "id",
    variables = c("SEX", "AGE"),
    retype = TRUE
  )
  testthat::expect_error(
    .check_prepared_dfs(
      prepared_dfs,
      subject_identifier = "id"
    ),
    regexp = "^.*Data frame 2 has no columns after filtering, please check your variables*$"
  )
  new_dfs <- .prepare_dfs(
    list(
      ist_1,
      ist_2
    ),
    subject_identifier = "id",
    variables = c("SEX", "AGE", "HOSPNUM", "RDELAY"),
    retype = TRUE
  )
  testthat::expect_true(
    all(
      "id" %in% colnames(new_dfs[[1]]),
      "id" %in% colnames(new_dfs[[2]]),
      ncol(new_dfs[[1]]) == 3,
      ncol(new_dfs[[2]]) == 3
    )
  )
})
