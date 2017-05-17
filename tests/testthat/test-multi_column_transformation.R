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

if (requireNamespace("microbenchmark", quietly = TRUE)) {
  describe("Benchmarks", {

    # This is technically a benchmark but I have no place to put it yet
    test_that("it triples a column no more than 7x as slow as a raw operation", {
      raw_triple <- function(dataframe, cols) {
        class(dataframe) <- "list"
        for (col in cols) dataframe[[paste0(col, "2")]] <- 3 * dataframe[[col]]
        class(dataframe) <- "data.frame"
        dataframe
      }
      tripler <- mungebit$new(multi_column_transformation(function(x) { 3 * x }))
      expect_equal(
        tripler$run(iris, "Sepal.Length", "Sepal.Length2"),
        raw_triple(iris, "Sepal.Length")
      )

      speeds  <- summary(microbenchmark::microbenchmark(
        tripler$run(iris, "Sepal.Length", "Sepal.Length2"),
        raw_triple(iris, "Sepal.Length"), times = 5L))
      multi_column_transformation_runtime <- speeds$median[[1L]]
      apply_raw_function_runtime    <- speeds$median[[2L]]
    
      # TODO: (RK) Figure out why this is 2x slower on Linux rather than on OSX.
      expect_true(multi_column_transformation_runtime < 12 * apply_raw_function_runtime,
        paste0("Execution of ", crayon::blue("multi_column_transformation"),
         " took too long: \nFormer took ",
         crayon::red(paste0(multi_column_transformation_runtime, "ms")), 
         " but latter took ",
         crayon::red(paste0(apply_raw_function_runtime, "ms")), ".\n",
         "You need to make sure the code for multi_column_transformation\n",
         "stays efficient relative to ",
         crayon::blue("raw_triple"),
         " (see code for this unit test)"))
    })
  })
}

