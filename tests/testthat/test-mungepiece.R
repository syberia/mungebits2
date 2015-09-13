## Simple mungepiece tests.
context("mungepiece")
library(testthatsomemore)

describe("errors", {
  test_that("it cannot run a mungepiece without arguments", {
    mb <- mungebit$new()
    mp <- mungepiece$new(mb)
    expect_error(mp$run(), "is missing, with no default")
  })

  test_that("it cannot initialize a mungepiece with a non-mungebit", {
    expect_error(mungepiece$new(1), "as the first argument")
    expect_error(mungepiece$new(NULL), "as the first argument")
    expect_error(mungepiece$new(identity), "as the first argument")
  })

  test_that("it cannot initialize mungepiece training args with a non-list", {
    mb <- mungebit$new()
    expect_error(mungepiece$new(mb, 1), "as the second argument")
    expect_error(mungepiece$new(mb, NULL), "as the second argument")
    expect_error(mungepiece$new(mb, identity), "as the second argument")
  })

  test_that("it cannot initialize mungepiece training args with a non-list", {
    mb <- mungebit$new()
    expect_error(mungepiece$new(mb, list(), 1), "as the third argument")
    expect_error(mungepiece$new(mb, list(), NULL), "as the third argument")
    expect_error(mungepiece$new(mb, list(), identity), "as the third argument")
  })
})
  
describe("simple calls", {
  make_fn <- function(train) {
    function(data, first, ...) {
      list(train = train, first = first, dots = list(...),
           first_expr = substitute(first),
           dots_expr = eval(substitute(alist(...))))
    }
  }

  make_bit   <- function() { mungebit$new(make_fn(TRUE), make_fn(FALSE)) }
  make_piece <- function(...) { mungepiece$new(make_bit(), ...) }

  test_that("it can create a mungepiece without error", {
    testthatsomemore::assert(make_piece())
  })
})


