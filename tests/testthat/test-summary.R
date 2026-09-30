testthat::test_that("summary works", {
  marginal_summary <- summary(marginal_distributions)
  sub_marginals <- marginal_distributions[[1]]
  # Expect a summary.RESIDE object
  expect_s3_class(marginal_summary, "summary.RESIDE")
  # Expect the overall summary
  expect_equal(marginal_summary$overall$n_data_frames, 1)
  expect_equal(marginal_summary$overall$subject_identifier, "")
  expect_equal(
    marginal_summary$overall$n_subjects,
    marginal_distributions$overall_summary$n_subjects
  )
  expect_true(marginal_summary$single_table)
  # Expect a row for the data frame
  data_frames <- marginal_summary$data_frames
  expect_equal(nrow(data_frames), 1)
  expect_equal(data_frames$n_row, sub_marginals$summary$n_row)
  expect_equal(data_frames$n_subjects, sub_marginals$summary$n_subjects)
  # Expect the number of each type of variable
  expect_equal(data_frames$n_variables, 6)
  expect_equal(data_frames$n_categorical, 2)
  expect_equal(data_frames$n_binary, 2)
  expect_equal(data_frames$n_continuous, 2)
  expect_equal(data_frames$n_dates, 0)
  # Expect only RATRIAL to have missing data
  expect_equal(data_frames$n_missing, 1)
  # Expect error when not class RESIDE
  expect_error(
    summary.RESIDE(list()),
    regexp = "object must be of class RESIDE",
    fixed = TRUE
  )
})

testthat::test_that("summary works with multiple tables", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  marginal_summary <- summary(multitable_marginals)
  # Expect the overall summary
  expect_equal(marginal_summary$overall$n_data_frames, 3)
  expect_equal(marginal_summary$overall$subject_identifier, "USUBJID")
  expect_equal(marginal_summary$overall$n_subjects, 306)
  expect_equal(marginal_summary$overall$common_columns, "STUDYID")
  expect_false(marginal_summary$single_table)
  # Expect a row for each data frame
  data_frames <- marginal_summary$data_frames
  expect_equal(data_frames$data_frame, c("dm", "cm", "ae"))
  for (df_name in data_frames$data_frame) {
    sub_marginals <- multitable_marginals[[df_name]]
    row <- data_frames[data_frames$data_frame == df_name, ]
    expect_equal(row$n_row, sub_marginals$summary$n_row)
    expect_equal(row$n_subjects, sub_marginals$summary$n_subjects)
    expect_equal(
      row$n_variables,
      length(get_submarginal_variables(sub_marginals))
    )
    expect_equal(
      row$n_categorical,
      length(sub_marginals$categorical_variables)
    )
    expect_equal(row$n_binary, length(sub_marginals$binary_variables))
    expect_equal(
      row$n_continuous,
      length(sub_marginals$continuous_variables)
    )
    expect_equal(
      row$n_variables,
      row$n_categorical + row$n_binary + row$n_continuous
    )
  }
  # Expect the number of subjects of each data frame
  expect_equal(data_frames$n_subjects, c(306, 229, 225))
  # Expect the date variables to be counted
  expect_equal(
    data_frames$n_dates[data_frames$data_frame == "dm"],
    length(get_dates_from_sub_marginals(multitable_marginals$dm))
  )
  expect_true(all(data_frames$n_dates > 0))
  expect_true(all(data_frames$n_missing > 0))
})

testthat::test_that("print.summary works", {
  # Expect the print to be invisible and return the summary
  marginal_summary <- summary(marginal_distributions)
  capture.output(
    returned <- expect_invisible(print(marginal_summary))
  )
  expect_identical(returned, marginal_summary)
  output <- capture.output(print(marginal_summary))
  # Expect the overall summary
  expect_true(
    any(grepl("Summary of Marginal Distributions", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Number of Subjects: 19435", output, fixed = TRUE))
  )
  # Expect the table of the data frame, without a data frame name
  expect_true(
    any(grepl("Rows", output, fixed = TRUE))
  )
  expect_false(
    any(grepl("Data Frame", output, fixed = TRUE))
  )
  expect_false(
    any(grepl("Subject Identifier:", output, fixed = TRUE))
  )
  # Expect a higher level summary than print
  expect_lt(
    length(output),
    length(capture.output(print(marginal_distributions)))
  )
})

testthat::test_that("print.summary works with multiple tables", {
  testthat::skip_if_not_installed("pharmaversesdtm")
  output <- capture.output(print(summary(multitable_marginals)))
  # Expect the overall summary
  expect_true(
    any(grepl("Number of Data Frames: 3", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Subject Identifier: USUBJID", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Common Columns: STUDYID", output, fixed = TRUE))
  )
  # Expect a row for each data frame
  expect_true(
    any(grepl("Data Frame", output, fixed = TRUE))
  )
  for (df_name in c("dm", "cm", "ae")) {
    expect_true(
      any(grepl(paste0("^\\s*", df_name, "\\s"), output))
    )
  }
  # Expect a higher level summary than print
  expect_lt(
    length(output),
    length(capture.output(print(multitable_marginals)))
  )
})
