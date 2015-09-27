context("parse_mungepiece")

describe("Invalid inputs", {
  test_that("it breaks when it does not receive a list", {
    expect_error(parse_mungepiece(5))
    expect_error(parse_mungepiece(identity))
    expect_error(parse_mungepiece(iris))
  })

  test_that("it breaks when you pass a list of length 0", {
    expect_error(parse_mungepiece(list()), "is not TRUE")
  })
})

train_fn   <- function(data, by = 2) {
  data[[1]] <- by * data[[1]];
  if (!exists("by", envir = input, inherits = FALSE)) {
    input$by <- by
  }
  data
}

predict_fn <- function(data, ...) { data[[1]] <- input$by * data[[1]]; data }

describe("First format", {
  test_that("it correctly creates a mungepiece using the first format with no additional arguments", {
    piece  <- parse_mungepiece(list(train_fn, 2))
    piece2 <- mungepiece$new(mungebit$new(train_fn), list(2))
    expect_same_piece(piece, piece2)
  })
  
  test_that("it can extract the train function from a mungebit with the first format", {
    mb <- mungebit$new(train_fn)
    piece  <- parse_mungepiece(list(mb, 2))
    piece2 <- mungepiece$new(mungebit$new(train_fn), list(2))
    expect_same_piece(piece, piece2)
  })
})

describe("Second format", {
  test_that("it correctly creates a mungepiece using the second format with an additional argument", {
    predict_fn2 <- function(data, by) { data[[1]] <- by * data[[1]]; data }
    piece  <- parse_mungepiece(list(list(train_fn, predict_fn2), 2))
    piece2 <- mungepiece$new(mungebit$new(train_fn, predict_fn2), list(2))
    expect_same_piece(piece, piece2)
  })

  test_that("it correctly runs a mungepiece using the second format with an additional argument", {
    predict_fn2 <- function(data, by) { cat("Foo", by); data[[1]] <- by * data[[1]]; data }
    piece  <- parse_mungepiece(list(list(train_fn, predict_fn2), 2))
    piece$run(iris)
    expect_output(piece$run(iris), "Foo 2")
  })
})

describe("Third format", {
  describe("Invalid inputs", {
    test_that("it errors if you provide less or more than 2 keys", {
      expect_error(parse_mungepiece(list(a = 1), "of length 1"))
      expect_error(parse_mungepiece(list(a = 1, b = 2, c = 3), "of length 3"))
    })

    test_that("it errors if you provide invalid keys", {
      expect_error(parse_mungepiece(list(train = 1, predic = 2), "a list with keys"))
      expect_error(parse_mungepiece(list(trai = 1, predict = 2), "a list with keys"))
      expect_error(parse_mungepiece(list(bar = 1, foo = 2), "a list with keys"))
    })

    test_that("it errors if there are no unnamed arguments on the train side", {
      expect_error(parse_mungepiece(list(train = list(a = 1), predict = list(2))),
                                    "at least one unnamed element on the train")
    })

    test_that("it errors if there are no unnamed arguments on the predict side", {
      expect_error(parse_mungepiece(list(train = identity, predict = list(a = 2))),
                                    "at least one unnamed element on the predict")
    })

    test_that("it errors if there is no function on the train side", {
      expect_error(parse_mungepiece(list(train = list(1), predict = list(2))),
                                    "must be a function")
    })

    test_that("it errors if there is no function on the predict side", {
      expect_error(parse_mungepiece(list(train = list(identity), predict = list(2))),
                                    "must be a function")
    })
  })

  test_that("the third format passes for a simple example", {
    reference <- mungepiece$new(mungebit$new(train_fn, predict_fn), list(by = 3), list(by = 1))
    actual    <- parse_mungepiece(list(train = list(train_fn, by = 3), predict = list(predict_fn, by = 1)))
    expect_same_piece(actual, reference)
  })

})

describe("Passing existing mungebit or mungepiece", {
  describe("Passing a mungepiece", {
    test_that("it accepts a mungepiece", {
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn), list(by = 3), list(by = 1))
      expect_equal(parse_mungepiece(list(piece)), piece)
      expect_same_piece(parse_mungepiece(list(piece)), piece)
    })

    test_that("it accepts a mungepiece and untrains it", {
      predict_fn2 <- function(data, by) { data[[1]] <- by * data[[1]]; data }
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn2), list(by = 3), list(by = 1))
      piece$run(iris)
      expect_not_same_piece(parse_mungepiece(list(piece)), piece, predict = FALSE)
    })
  })

  describe("Passing a mungebit", {
    test_that("it accepts a mungebit", {
      bit <- mungebit$new(train_fn, predict_fn)
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn))
      expect_equal(parse_mungepiece(list(bit)), piece)
      expect_same_piece(parse_mungepiece(list(bit)), piece)
    })

    test_that("it accepts a mungebit and untrains it", {
      predict_fn2 <- function(data, by = 5) { data[[1]] <- by * data[[1]]; data }
      bit <- mungebit$new(train_fn, predict_fn2)
      bit$run(iris, by = 3)
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn2))
      expect_same_piece(parse_mungepiece(list(bit)), piece)
    })
  })

  describe("Passing a mungebit with arguments", {
    test_that("it accepts a mungebit", {
      bit <- mungebit$new(train_fn, predict_fn)
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn), list(by = 3))
      expect_equal(parse_mungepiece(list(bit, by = 3)), piece)
      expect_same_piece(parse_mungepiece(list(bit, by = 3)), piece)
    })

    test_that("it accepts a mungebit and untrains it", {
      predict_fn2 <- function(data, by = 5) { data[[1]] <- by * data[[1]]; data }
      bit <- mungebit$new(train_fn, predict_fn2)
      bit$run(iris, by = 3)
      piece <- mungepiece$new(mungebit$new(train_fn, predict_fn2), list(by = 3))
      expect_same_piece(parse_mungepiece(list(bit, by = 3)), piece)
    })
  })
})


