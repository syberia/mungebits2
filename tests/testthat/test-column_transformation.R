context("column transformation")

describe("Simplest examples", {
  test_that("it can run an identity column transformation", {
    expect_equal(mungebit$new(column_transformation(identity))$run(iris), iris)
  })

  test_that("correctly transforms one column by multiplying by two", {
    doubler <- mungebit$new(column_transformation(function(x) { 2 * x }))
    iris2   <- doubler$run(iris, "Sepal.Length")
    expect_equal(iris2[TRUE], transform(iris, Sepal.Length = 2 * Sepal.Length),
                 info = "column_transformation must double first column of iris")
  })

  test_that("correctly transforms multiple columns by multiplying by two", {
    doubler <- column_transformation(function(x) { 2 * x })
    iris2   <- mungebit$new(doubler)$run(iris, c("Sepal.Length", "Sepal.Width"))
    expect_equal(iris2[TRUE], transform(iris, Sepal.Length = 2 * Sepal.Length, Sepal.Width = 2 * Sepal.Width),
                 info = paste("column_transformation must double first",
                              "2 columns of iris"))
  })

  test_that("correctly transforms one column by converting to character", {
    stringer <- mungebit$new(column_transformation(as.character))
    iris2    <- stringer$run(iris, "Sepal.Length")
    expect_equal(iris2, transform(iris, Sepal.Length = as.character(Sepal.Length)),
                 info = paste("column_transformation must convert to character",
                              "first column of iris"))
  })

  test_that('correctly transforms multiple columns by converting to character', {
    stringer <- column_transformation(as.character)
    iris2    <- mungebit$new(stringer)$run(iris, colnames(iris))
    char_dataframe <- data.frame(vapply(iris, as.character, character(NROW(iris))),
                          stringsAsFactors = FALSE)
    expect_equal(iris2, char_dataframe,
                 info = paste("column_transformation must convert to character",
                              "all columns of iris"))
  })

})
