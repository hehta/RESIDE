test_that("check_df errors when not coercible to df", {
  expect_false(check_df(NULL))
})
