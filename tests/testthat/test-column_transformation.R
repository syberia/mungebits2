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

  test_that("it correctly runs column transformations using a function as column name specifier", {
    doubler <- mungebit$new(column_transformation(function(x) { 2 * x }))
    iris2   <- doubler$run(iris, is.numeric)
    expect_equal(iris2[, 1:4], iris[, 1:4] * 2)
  })

})

describe("Passing arguments", {

  test_that("it correctly passes dots arguments", {
    scaler <- mungebit$new(column_transformation(function(x, v) { v * x }))
    iris2  <- scaler$run(iris[1:4], , 2)
    expect_equal(iris2[1:4], 2 * iris[1:4],
                 info = "column_transformation must double first column of iris")
  })

  test_that('accepts transformation calls with missing arguments', {
    doubler <- mungebit$new(column_transformation(function(x) { 2 * x }))
    iris2   <- doubler$run(iris[1: 4])
    expect_equal(iris2, 2 * iris[, 1:4],
                 info = "column_transformation must double first column of iris")
  })

  describe("Nonstandard evaluation pass-along", {
    it("passes along nonstandard evaluation", {
      nse <- mungebit$new(nse = TRUE, column_transformation(nonstandard = TRUE, function(x) {
        paste0(deparse(substitute(x)), x) }))
      balloo <- iris
      iris2  <- nse$run(balloo, 5)
      expect_equal(iris2, transform(iris, Species = paste0('balloo[["Species"]]', Species)),
                   info = "column_transformation should pass along non-standard evaluation")
    })
  })

  describe("Nonstandard evaluation pass-along with a call rather than a name", {
    it("passes along nonstandard evaluation", {
      nse <- mungebit$new(nse = TRUE, column_transformation(nonstandard = TRUE, function(x) {
        paste0(deparse(substitute(x)), x) }))
      balloo <- function() iris
      iris2  <- nse$run(balloo(), 5)
      expect_equal(iris2, transform(iris, Species = paste0('balloo()[["Species"]]', Species)),
                   info = "column_transformation should pass along non-standard evaluation")
    })
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

describe("dropping columns", {
  test_that("it can drop columns using a column_transformation", {
    dropper <- mungebit$new(column_transformation(function(x) NULL))
    browser()
    expect_equal(dropper$run(iris, 1), iris[-1])
  })
})

if (requireNamespace("microbenchmark", quietly = TRUE)) {
  describe("Benchmarks", {

    # This is technically a benchmark but I have no place to put it yet
    test_that("it doubles a column no more than 6x as slow as a raw operation", {
      raw_double <- function(dataframe, cols) {
        class(dataframe) <- "list"
        for (col in cols) dataframe[[col]] <- 2 * dataframe[[col]]
        class(dataframe) <- "data.frame"
        dataframe
      }
      doubler <- mungebit$new(column_transformation(function(x) { 2 * x }))
      speeds  <- summary(microbenchmark::microbenchmark(
        doubler$run(iris, 1:4), raw_double(iris, 1:4), times = 5L))
      column_transformation_runtime <- speeds$median[[1L]]
      apply_raw_function_runtime    <- speeds$median[[2L]]
    
      expect_true(column_transformation_runtime < 6 * apply_raw_function_runtime,
        paste0("Execution of ", crayon::blue("column_transformation"),
         " took too long: \nFormer took ",
         crayon::red(paste0(column_transformation_runtime, "ms")), 
         " but latter took ",
         crayon::red(paste0(apply_raw_function_runtime, "ms")), ".\n",
         "You need to make sure the code for column_transformation\n",
         "stays efficient relative to ",
         crayon::blue("raw_double"),
         " (see code for this unit test)"))
    })
  })
}

describe("debugging", {
  test_that("calling debug on a column transformation sets the debug flag", {
    ct <- column_transformation(identity)
    debug(ct)
    expect_true(isdebugged(get("transformation", envir = environment(ct))))
  })

  test_that("calling undebug on a column transformation unsets the debug flag", {
    ct <- column_transformation(identity)
    debug(ct)
    undebug(ct)
    expect_false(isdebugged(get("transformation", envir = environment(ct))))
  })
})


