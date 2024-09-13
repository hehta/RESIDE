testthat::test_that("get_continuous Summary works", {
  testthat::expect_warning(get_continuous_summary(IST["SEX"]),
    regexp = "^.*Could not transform variable SEX.+skipping*.$"
  )
})