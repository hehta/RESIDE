data(IST)

testthat::test_that("get_missing_variables works", {
  testthat::expect_equal(
    length(get_missing_variables(IST, "AGE")),
    0
  )
  testthat::expect_equal(
    length(get_missing_variables(IST, "test")),
    1
  )
  testthat::expect_equal(
    get_missing_variables(IST, "test"),
    c("test")
  )
})
