context("multi column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(multi_column_transformation(identity))$run(iris, "Sepal.Length", "Sepal.Length"), iris)
  })

})

