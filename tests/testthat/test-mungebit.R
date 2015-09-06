## Simple mungebit tests.
context("mungebit")

test_that("it correctly sets the trained flag after the first run", {
  mb <- mungebit$new()
  mb$run(iris)
  expect_true(mb$trained())
})

test_that("it correctly executes training and prediction functions", {
  env <- new.env()
  mb <- mungebit$new(function(d) env$trained <- TRUE, function(d) env$predicted <- TRUE)
  mb$run(iris)
  expect_true(env$trained)
  expect_null(env$predicted)
  mb$run(iris)
  expect_true(env$trained)
  expect_true(env$predicted)
})

test_that("it sets input correctly", {
  mb <- mungebit$new(function(d) input$trained <- TRUE)
  mb$run(iris)
  expect_true(mb$input()$trained)
})

test_that("it can execute NULL functions", {
  mb <- mungebit$new(NULL, NULL)
  testthatsomemore::assert(mb$run(iris))
  testthatsomemore::assert(mb$run(iris))
})

test_that("it errors if we attempt to modify input after training", {
  mb <- mungebit$new(NULL, function(d) input$foo <- TRUE)          
  testthatsomemore::assert(mb$run(iris))
  expect_error(mb$run(iris), "cannot add bindings to a locked environment")
})

test_that("it can read from input during predict", {
  other <- new.env()
  mb <- mungebit$new(function(d) input$foo <- TRUE, function(d) other$foo <- input$foo) 
  mb$run(iris)
  mb$run(iris)
  expect_identical(as.list(other), mb$input())
})

test_that("it can take variadic arguments in its train function", {
  mb <- mungebit$new(function(d, foo) input$foo <- foo)
  mb$run(iris, foo = "bar")
  expect_equal(mb$input()$foo, "bar")
})

test_that("it can take variadic arguments in its predict function", {
  other <- new.env()
  mb <- mungebit$new(function(d, foo) input$foo <- foo,
                     function(d, baz) other$baz <- c(input$foo, baz))
  mb$run(iris, foo = "bar")
  mb$run(iris, baz = "baz")
  expect_equal(other$baz, c("bar", "baz"))
})

test_that("it can sustain nonstandard evaluation in train", {
  mb <- mungebit$new(function(d, foo) input$foo <- substitute(foo))
  mb$run(iris, foo = hello + world)
  expect_identical(mb$input()$foo, quote(hello + world))
})

