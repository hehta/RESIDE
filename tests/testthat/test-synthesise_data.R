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
