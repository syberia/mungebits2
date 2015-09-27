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
  })



})


