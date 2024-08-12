data(ist)

test_that("get_missing_variables works", {
  expect_equal(
    length(get_missing_variables(IST, "AGE")),
    0
  )
  expect_equal(
    length(get_missing_variables(IST, "test")),
    1
  )
  expect_equal(
    get_missing_variables(IST, "test"),
    c("test")
  )
})
