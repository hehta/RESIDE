testthat::test_that("print works", {
  # Expect the print to be invisible and return the object
  capture.output(
    returned <- expect_invisible(print(marginal_distributions))
  )
  expect_identical(returned, marginal_distributions)
  # Expect the overall summary
  expect_output(
    print(marginal_distributions),
    regexp = "Summary of Marginal Distributions",
    fixed = TRUE
  )
  # Expect each variable type
  expect_output(
    print(marginal_distributions),
    regexp = "Summary of Categorical Variables",
    fixed = TRUE
  )
  expect_output(
    print(marginal_distributions),
    regexp = "Summary of Binary Variables",
    fixed = TRUE
  )
  expect_output(
    print(marginal_distributions),
    regexp = "Summary of Continuous Variables",
    fixed = TRUE
  )
  # Expect the variables
  expect_output(
    print(marginal_distributions),
    regexp = "Variable: SEX",
    fixed = TRUE
  )
  # Expect no data frame names or subject identifier for a single table
  output <- capture.output(print(marginal_distributions))
  expect_false(
    any(grepl("Data Frame:", output, fixed = TRUE))
  )
  expect_false(
    any(grepl("Subject Identifier:", output, fixed = TRUE))
  )
  # Expect categories to keep their order when not capped
  expect_lt(
    grep("  F : ", output, fixed = TRUE),
    grep("  M : ", output, fixed = TRUE)
  )
  expect_false(
    any(grepl("more categories", output, fixed = TRUE))
  )
  # Expect error when not class RESIDE
  expect_error(
    print.RESIDE(list()),
    regexp = "object must be of class RESIDE",
    fixed = TRUE
  )
})

testthat::test_that("print works with multiple tables", {
  output <- capture.output(print(multitable_marginals))
  # Expect the overall summary
  expect_true(
    any(grepl("Number of Data Frames: 3", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Data Frames: dm, cm, ae", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Subject Identifier: USUBJID", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("Common Columns: STUDYID", output, fixed = TRUE))
  )
  # Expect a section for each data frame
  for (df_name in c("dm", "cm", "ae")) {
    expect_true(
      any(grepl(paste("Data Frame:", df_name), output, fixed = TRUE))
    )
  }
  # Expect no binary section, as there are no binary variables
  expect_false(
    any(grepl("Summary of Binary Variables", output, fixed = TRUE))
  )
  # Expect dates to be printed as dates
  expect_true(
    any(grepl("Date: TRUE", output, fixed = TRUE))
  )
  expect_true(
    any(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", output))
  )
  # Expect categorical variables to be capped to the 10 most common
  n_subjid <- length(multitable_marginals$dm$categorical_variables$SUBJID)
  expect_true(
    any(grepl(
      paste("... and", n_subjid - 10, "more categories"),
      output,
      fixed = TRUE
    ))
  )
  cm_trt <- multitable_marginals$cm$categorical_variables$CMTRT
  top_trt <- names(sort(cm_trt, decreasing = TRUE))[1]
  hidden_trt <- names(sort(cm_trt, decreasing = TRUE))[11]
  expect_true(
    any(grepl(paste(top_trt, ":"), output, fixed = TRUE))
  )
  expect_false(
    any(grepl(paste0("  ", hidden_trt, " :"), output, fixed = TRUE))
  )
  # Expect every category to be printed when full = TRUE
  full_output <- capture.output(print(multitable_marginals, full = TRUE))
  expect_false(
    any(grepl("more categories", full_output, fixed = TRUE))
  )
  expect_true(
    any(grepl(paste0("  ", hidden_trt, " :"), full_output, fixed = TRUE))
  )
  expect_gt(length(full_output), length(output))
})
