## Simple mungepiece tests.
context("mungepiece")
library(testthatsomemore)

describe("errors", {
  test_that("it cannot run a mungepiece without arguments", {
    mb <- mungebit$new()
    mp <- mungepiece$new(mb)
    expect_error(mp$run(), "is missing, with no default")
  })
})

