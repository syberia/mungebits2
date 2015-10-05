context("standard column format")

test_that("it correctly parses numeric input", {
  expect_equal(standard_column_format(c(1,5), iris),
               c("Sepal.Length", "Species"))
})

test_that("it correctly parses logical input", {
  expect_equal(standard_column_format(c(TRUE, FALSE, FALSE, FALSE, TRUE), iris),
               c("Sepal.Length", "Species"))
})

test_that("it correctly parses character input", {
  expect_equal(standard_column_format("Sepal.Length", iris), "Sepal.Length")
})

test_that("it correctly parses function input", {
  expect_equal(standard_column_format(is.numeric, iris),
               colnames(iris)[vapply(iris, is.numeric, logical(1))])

  mean_fn <- function(x) is.numeric(x) && mean(x, na.rm = TRUE) > 5
  expect_equal(standard_column_format(mean_fn, iris),
               colnames(iris)[vapply(iris, mean_fn, logical(1))])
})

test_that("it correctly parses list input", {
  cols <- standard_column_format(list(1:2, "Sepal.Width"), iris)
  expect_equal(cols, colnames(iris)[2])
})

test_that("it correctly parses nested list input", {
  cols <- standard_column_format(list(5, "Species", list(is.factor)), iris)
  expect_equal(cols, colnames(iris)[5])
})

test_that("it should be able to use except correctly with a character", {
  cols <- standard_column_format(except("Sepal.Length"), iris)
  expect_equal(cols, colnames(iris)[-1])
})

test_that("it should be able to use except correctly with a function", {
  cols <- standard_column_format(except(is.factor), iris)
  expect_equal(cols, colnames(iris)[-5])
})

test_that("it should be able to use except correctly with an integer", {
  cols <- standard_column_format(except(1), iris)
  expect_equal(cols, colnames(iris)[-1])
  cols <- standard_column_format(except(-1), iris)
  expect_equal(cols, colnames(iris)[1])
})

test_that("it should be able to use except correctly with a list", {
  cols <- standard_column_format(except(list(1:2, "Sepal.Width")), iris)
  expect_equal(cols, colnames(iris)[-2])
})

test_that("it should be able to use an AND condition with except", {
  cols <- standard_column_format(list(except("Sepal.Width"), ls(iris)), iris)
  expect_equal(cols, colnames(iris)[-2])
})

test_that("it can use except with two columns", {
  cols <- standard_column_format(list(except(c("Sepal.Width", "Sepal.Length")), ls(iris)), iris)
  expect_equal(cols, colnames(iris)[-c(1,2)])
})

test_that("it can accept named columns", {
  cols <- standard_column_format(function(., name) grepl("^Sep", name), iris)
  expect_equal(cols, grep("^Sep", colnames(iris), value = TRUE))
})

