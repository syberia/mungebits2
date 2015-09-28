context("munge")

test_that("it does nothing when no mungepieces are passed", {
  expect_equal(munge(iris, list()), iris[,])
})

