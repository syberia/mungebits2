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

