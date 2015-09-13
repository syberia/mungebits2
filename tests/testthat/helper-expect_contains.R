expect_contains <- function(obj, sublist, ...) {
  stopifnot(unnamed_count(obj) == 0, unnamed_count(sublist) == 0)
  expect_true(all(names(sublist) %in% names(obj)))
  expect_equal(obj[names(sublist)], sublist, ...)
}

