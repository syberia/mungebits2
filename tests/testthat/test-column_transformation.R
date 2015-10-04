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

  test_that("correctly transforms multiple columns by converting to character", {
    stringer <- column_transformation(as.character)
    iris2    <- mungebit$new(stringer)$run(iris, colnames(iris))
    char_dataframe <- data.frame(vapply(iris, as.character, character(NROW(iris))),
                          stringsAsFactors = FALSE)
    expect_equal(iris2, char_dataframe,
                 info = paste("column_transformation must convert to character",
                              "all columns of iris"))
  })

  test_that("correctly transforms a column into rounded factors", {
    round_and_factor <- column_transformation(function(x) { factor(round(x)) })
    iris2 <- mungebit$new(round_and_factor)$run(iris, "Sepal.Length")
    
    expect_equal(iris2, transform(iris, Sepal.Length = factor(round(Sepal.Length))),
                 info = paste("column_transformation must convert to factor",
                              "after rounding first column of iris"))
  })

  test_that("correctly transforms using numeric column indices", {
    doubler <- column_transformation(function(x) { 2 * x })
    iris2 <- mungebit$new(doubler)$run(iris, 2)
    
    expect_equal(iris2, transform(iris, Sepal.Width = 2 * Sepal.Width),
                 info = paste("column_transformation must be able to reference",
                              "columns using numeric indices",
                              "(e.g., doubler(iris,2)"))
  })

  test_that("correctly transforms using logical column indices", {
    doubler <- column_transformation(function(x) 2 * x)
    iris2   <- mungebit$new(doubler)$run(iris, "Sepal.Width" == colnames(iris))
    
    expect_equal(iris2, transform(iris, Sepal.Width = 2 * Sepal.Width),
                 info = paste("column_transformation must be able to reference",
                              "columns using numeric indices",
                              "(e.g., doubler(iris, c(F,T,F,F,F))"))
  })

  test_that("it correctly imputes means", {
    mean_imputer <- column_transformation(function(x) {
      x[is.na(x)] <- mean(x, na.rm = TRUE); x
    })
    iris2       <- iris
    iris2[1, 1] <- NA
    iris2       <- mungebit$new(mean_imputer)$run(iris2, 1)
    
    expect_equal(iris2, transform(iris, Sepal.Length = c(mean(Sepal.Length[-1L]), Sepal.Length[-1L])),
                 info = paste("column_transformation must impute NAs with mean"))
  })

})

describe("Passing arguments", {

  test_that("it correctly passes dots arguments", {
    scaler <- mungebit$new(column_transformation(function(x, v) { v * x }))
    iris2  <- scaler$run(iris[1:4], , 2)
    expect_equal(iris2[1:4], 2 * iris[1:4],
                 info = "column_transformation must double first column of iris")
  })

  describe("The name argument for named column transformations", {

    test_that("the name argument is respected", {
      paster <- mungebit$new(column_transformation(function(x, name) { paste0(x, "_", name) }))
      iris2  <- paster$run(iris[5])
      expect_equal(iris2, transform(iris[5], Species = paste0(Species, "_Species")),
                   info = "column_transformation must double first column of iris")
    })

  })

})

