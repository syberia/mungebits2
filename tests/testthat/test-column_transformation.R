context("column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(column_transformation(identity))$run(iris), iris)
  })

  test_that("correctly transforms one column by multiplying by two", {
    doubler <- mungebit$new(column_transformation(function(x) { 2 * x }))
    iris2   <- doubler$run(iris, "Sepal.Length")
    expect_equal(iris2[TRUE], transform(iris, Sepal.Length = 2 * Sepal.Length),
                 info = "column_transformation must double first column of iris2")
  })
})

