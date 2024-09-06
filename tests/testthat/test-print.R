testthat::test_that("print works as it should", {
  marginals <- get_marginal_distributions(
    IST,
    variables = c(
      "SEX",
      "AGE",
      "ID14",
      "RSBP",
      "RATRIAL",
      "SET14D"
    )
  )
  testthat::expect_output(print(marginals))
  testthat::expect_invisible(print(marginals))
})