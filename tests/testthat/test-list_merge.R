context("list_merge")

test_that("it can merge NULL with non-null", {
  expect_identical(list_merge(NULL, list(a = 1)), list(a = 1))
})

test_that("it can merge a simple example", {
  expect_identical(list_merge(list(a = 1, b = 2), list(b = 3, c = 4)),
                   list(a = 1, b = 3, c = 4))
})

test_that("it can merge with NULL", {
  expect_identical(list_merge(list(a = 1), NULL), list(a = 1))
  expect_identical(list_merge(NULL, NULL), list())
})

