data(ist)

test_that("get_missing_variables works", {
  expect_equal(
    length(get_missing_variables(ist, "AGE")),
    0
  )
  expect_equal(
    length(get_missing_variables(ist, "test")),
    1
  )
  expect_equal(
    get_missing_variables(ist, "test"),
    c("test")
  )
})
