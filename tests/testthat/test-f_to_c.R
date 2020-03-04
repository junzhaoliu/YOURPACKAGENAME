test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("f_to_c works mathmatically", {
  expect_equal(f_to_c(32), 0)
  expect_equal(f_to_c(212), 100)
})
test_that("non-numeric input throws error", {
  expect_error(f_to_c("a string"))
})
