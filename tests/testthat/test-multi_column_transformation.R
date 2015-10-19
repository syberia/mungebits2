context("multi column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(multi_column_transformation(identity))$run(iris, "Sepal.Length", "Sepal.Length"), iris)
  })

  test_that("it can run a divider", {
    expect_equal(mungebit$new(multi_column_transformation(`/`))$run(iris, c("Sepal.Length", "Sepal.Width"), "Sepal.Ratio"),
                 transform(iris, Sepal.Ratio = Sepal.Length / Sepal.Width))
  })

  test_that("it can swap column orders", {
    expect_equal(mungebit$new(multi_column_transformation(function(x, y) y / x))$run(iris, c("Sepal.Width", "Sepal.Length"), "Sepal.Ratio"),
                 transform(iris, Sepal.Ratio = Sepal.Length / Sepal.Width))
  })

})

describe("With inputs", {
  test_that("it can remember its inputs", {
    mean_diff <- function(x, y) {
      if (!isTRUE(trained)) {
        input$mean <- mean(x, na.rm = TRUE)
      }
      y - input$mean
    }
    iris2 <- iris
    iris2$Sepal.Out <- iris2$Sepal.Width - mean(iris2$Sepal.Length)
    mb <- mungebit$new(multi_column_transformation(mean_diff))
    out <- mb$run(iris, c("Sepal.Length", "Sepal.Width"), "Sepal.Out")
    expect_equal(out, iris2)
    expect_equal(mb$run(iris[1, ], c("Sepal.Length", "Sepal.Width"), "Sepal.Out"), iris2[1, ])
  })

})

