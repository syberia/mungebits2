context("parse_mungepiece")

describe("Invalid inputs", {
  test_that("it breaks when it does not receive a list", {
    expect_error(parse_mungepiece(5))
    expect_error(parse_mungepiece(identity))
    expect_error(parse_mungepiece(iris))
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


