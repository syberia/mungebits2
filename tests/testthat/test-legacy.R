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
})

