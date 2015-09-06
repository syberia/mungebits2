## Simple mungebit tests.
context("mungebit")

test_that("it correctly sets the trained flag after the first run", {
  mb <- mungebit$new()
  mb$run(iris)
  expect_true(mb$trained())
})


