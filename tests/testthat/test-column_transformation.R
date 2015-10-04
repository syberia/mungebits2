context("column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(column_transformation(identity))$run(iris), iris)
  })
})

