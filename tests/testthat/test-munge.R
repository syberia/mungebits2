context("munge")

describe("Invalid inputs", {
  test_that("the munge list must be a list", {
    expect_error(munge(iris, 5), "must be a list")
    expect_error(munge(iris, identity), "must be a list")
    expect_error(munge(iris, globalenv()), "must be a list")
  })

  test_that("when munging against a data.frame it must have a mungepieces attribute", {
    expect_error(munge(iris, beaver2), "must have a ")
  })
})

test_that("it does nothing when no mungepieces are passed", {
  expect_equal(munge(iris, list()), iris[,])
})

test_that("it correctly adds to the mungepieces list", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1))
  iris2 <- munge(iris, args)
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("it correctly munges a simple mungepiece sequence", {
  args <- lapply(seq_len(2) + 1,
    function(x) list(function(x, v) { x[[1]] <- v * x[[1]]; x }, x))
  iris2 <- munge(iris, args)
  expect_equal(iris2[[1]], iris[[1]] * 6)
})


