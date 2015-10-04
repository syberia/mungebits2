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

  test_that("correctly transforms multiple columns by multiplying by two", {
    doubler <- column_transformation(function(x) { 2 * x })
    iris2   <- mungebit$new(doubler)$run(iris, c("Sepal.Length", "Sepal.Width"))
    expect_equal(iris2[TRUE], transform(iris, Sepal.Length = 2 * Sepal.Length, Sepal.Width = 2 * Sepal.Width),
                 info = paste("column_transformation must double first",
                              "2 columns of iris2"))
  })

})

