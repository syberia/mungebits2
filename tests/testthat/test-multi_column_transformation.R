context("multi column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(multi_column_transformation(identity))$run(iris, "Sepal.Length", "Sepal.Length"), iris)
  })

  test_that("it can run a divider", {
    expect_equal(mungebit$new(multi_column_transformation(`/`))$run(iris, c("Sepal.Length", "Sepal.Width"), "Sepal.Ratio"),
                 transform(iris, Sepal.Ratio = Sepal.Length / Sepal.Width))
  })

})

