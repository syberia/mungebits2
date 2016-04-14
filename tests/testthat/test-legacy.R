# This file tests that the munge function has backwards compatibility
# with legacy mungebits: https://github.com/robertzk/mungebits
context("legacy mungebits")
# Note we do not wish to attach mungebits to the search path,
# so we merely `requireNamespace`.
requireNamespace("mungebits", quietly = TRUE)

describe("Munging with legacy mungebits", {
  test_that("we can use the new munge function with legacy mungebits", {
    mb <- mungebits:::mungebit$new(function(df) { eval.parent(substitute({ df[[1]] <- NULL; })) })
    mp <- mungebits:::mungepiece$new(mb)
    iris2 <- munge(iris, list(mp, mp))
    attr(iris2, "mungepieces") <- NULL
    expect_equal(iris2, iris[, 3:5],
      info = paste0("The first two columns of iris should have been dropped ",
                    "with a legacy mungebit."))
  })
  
  test_that("we can use the new munge function with mixed new and legacy mungebits", {
    mb <- mungebits:::mungebit$new(function(df) { eval.parent(substitute({ df[[1]] <- NULL; })) })
    mp <- mungebits:::mungepiece$new(mb)
    newmp <- mungepiece$new(mungebit$new(function(df) { df[[3]] <- df[[1]] + df[[2]]; df }))
    iris2 <- munge(iris, list(mp, newmp))
    attr(iris2, "mungepieces") <- NULL
    expected_iris <- iris[, 2:5]; expected_iris[[3]] <- expected_iris[[1]] + expected_iris[[2]]
    expect_equal(iris2, expected_iris)

    # Check that mixed new and legacy munging is order-independent
    iris2 <- munge(iris, list(newmp, mp))
    attr(iris2, "mungepieces") <- NULL
    expected_iris <- iris; expected_iris[[3]] <- expected_iris[[1]] + expected_iris[[2]]
    expected_iris[[1]] <- NULL
    expect_equal(iris2, expected_iris)
  })

  test_that("legacy mungebits work as expected in predict", {
    mb <- mungebits:::mungebit$new(function(df) { eval.parent(substitute({
      if (!is.element("levels", ls(inputs))) {
        inputs$levels <<- levels(df[[5]])
      } else {
        levels(df[[5]]) <- inputs$levels
      }
      df
    })) })
    mp <- mungebits:::mungepiece$new(mb)
    iris2 <- munge(iris, list(mp))
    iris1 <- iris[1, ]; iris1[[5]] <- as.factor(as.character(iris1[[5]]))
    expect_equal(levels(munge(iris1, iris2)[[5]]), levels(iris[[5]]))
  })
})

describe("Creating legacy mungebits using the munge function", {
  test_that("combining legacy and non-legacy functions is illegal", {
    legacy_fn <- function(x) x; class(legacy_fn) <- "legacy_mungebit_function"
    expect_error(munge(iris, list(list(list(legacy_fn, identity)))),
                 "Cannot mix new and legacy")
    expect_error(munge(iris, list(list(list(identity, legacy_fn)))),
                 "Cannot mix new and legacy")
  })

  test_that("we can use the new munge function with legacy mungebits", {
    legacy_fn <- function(df) {
      eval.parent(substitute({ df[[1]] <- NULL }))
    }
    # This must be a legacy function.
    expect_error(munge(iris, list(list(legacy_fn))))
    class(legacy_fn) <- "legacy_mungebit_function"
    iris2 <- munge(iris, list(list(legacy_fn)))
    attr(iris2, "mungepieces") <- NULL
    expect_equal(iris2, iris[-1])
  })

  test_that("we can use the new munge function with mixed legacy and non-legacy mungebits", {
    legacy_fn <- function(df) {
      eval.parent(substitute({ df[[1]] <- NULL }))
    }
    class(legacy_fn) <- "legacy_mungebit_function"
    non_legacy_fn <- function(df) { df[[1]] <- NULL; df }
    iris2 <- munge(iris, list(list(legacy_fn), list(non_legacy_fn)))
    attr(iris2, "mungepieces") <- NULL
    expect_equal(iris2, iris[-c(1,2)])
  })
})


