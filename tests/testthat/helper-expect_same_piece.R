expect_equal2 <- function(obj1, obj2, negate = FALSE) {
  if (isTRUE(negate)) {
    expect_false(isTRUE(all.equal(obj1, obj2)))
  } else {
    expect_true(isTRUE(all.equal(obj1, obj2)))
  }
}

#' Expect two mungepieces to have the same effect.
#'
#' @param piece1 mungepiece.
#' @param piece2 mungepiece.
#' @param data data.frame. The dataset to run the mungepiece on. By default,
#'    \code{\link[datasets]{iris}}.
#' @param train_args list. The arguments to pass to the mungepieces during
#'    training, by default none.
#' @param predict_args list. The arguments to pass to the mungepieces during
#'    prediction, by default none.
#' @param predict logical. Whether or not to test on predict as well, by default
#'    \code{TRUE}.
#' @param fail logical. If \code{TRUE}, reverse the expectation, so that the
#'    pieces should not have identical effects.
expect_same_piece <- function(piece1, piece2, data = iris,
                              train_args = list(), predict_args = list(),
                              train = TRUE, predict = TRUE, fail = FALSE) {
  stopifnot(is.mungepiece(piece1), is.mungepiece(piece2))

  data1 <- do.call(piece1$run, c(list(data), train_args))
  data2 <- do.call(piece2$run, c(list(data), train_args))
  if (isTRUE(train)) {
    expect_equal2(data1, data2, negate = isTRUE(fail))
  }

  if (isTRUE(predict)) {
    # The mungepieces should be trained already.
    data1 <- do.call(piece1$run, c(list(data), train_args))
    data2 <- do.call(piece2$run, c(list(data), train_args))
    expect_equal2(data1, data2, negate = isTRUE(fail))
  }

  TRUE
}

expect_not_same_piece <- function(..., fail) {
  expect_same_piece(..., fail = TRUE)
}

