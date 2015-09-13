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

make_fn <- function(train) {
  function(data, first = NULL, ...) {
    list(train = train, first = first, dots = list(...),
         first_expr = substitute(first),
         dots_expr = eval(substitute(alist(...))))
  }
}

make_bit   <- function() { mungebit$new(make_fn(TRUE), make_fn(FALSE)) }
make_piece <- function(...) { mungepiece$new(make_bit(), ...) }
  
describe("without default arguments", {
  test_that("it can create a mungepiece without error", {
    testthatsomemore::assert(make_piece())
  })

  describe("with no arguments", {
    test_that("it can train a mungepiece without error", {
      testthatsomemore::assert(make_piece()$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece()
      piece$run(iris)
      expect_true(piece$mungebit()$trained())
    })
  })

  describe("with variable arguments", {
    describe("during training", {
      test_that("it can train with variable arguments", {
        testthatsomemore::assert(make_piece()$run(iris, "foo", "bar"))
      })

      test_that("it captures the expected values during train", {
        expect_contains(make_piece()$run(iris, "foo", "bar"),
                        list(train = TRUE, first = "foo", dots = list("bar")))
      })

      test_that("it captures expressions during train", {
        x <- "fo"
        expect_contains(make_piece()$run(iris, paste0(x, "o"), identity("bar")),
                        list(train = TRUE, first = "foo", dots = list("bar"),
                             first_expr = quote(paste0(x, "o")),
                             dots_expr = list(quote(identity("bar")))))
      })
    })

    describe("during prediction", {
      test_that("it can predict with variable arguments", {
        piece <- make_piece()
        piece$run(iris, "foo", "bar")
        testthatsomemore::assert(piece$run(iris, "foo", "bar"))
      })

      test_that("it captures the expected values during train", {
        piece <- make_piece()
        piece$run(iris)
        expect_contains(piece$run(iris, "foo", "bar"),
                        list(train = FALSE, first = "foo", dots = list("bar")))
      })

      test_that("it captures expressions during train", {
        piece <- make_piece()
        piece$run(iris)
        x <- "fo"
        expect_contains(piece$run(iris, paste0(x, "o"), identity("bar")),
                        list(train = FALSE, first = "foo", dots = list("bar"),
                             first_expr = quote(paste0(x, "o")),
                             dots_expr = list(quote(identity("bar")))))
      })
    })
  })
})

describe("with unnamed default arguments", {
  make_piece2 <- function() {
    make_piece(list("Stephen", "Colbert"), list("Jon", "Stewart"))
  }

  test_that("it can create a mungepiece without error", {
    testthatsomemore::assert(make_piece2())
  })

  describe("without arguments", {
    test_that("it can train a mungepiece without error", {
      testthatsomemore::assert(make_piece2()$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece2()
      piece$run(iris)
      expect_true(piece$mungebit()$trained())
    })
  })

  describe("with unnamed variable arguments", {
    describe("during training", {
      test_that("it can train with unnamed variable arguments", {
        testthatsomemore::assert(make_piece2()$run(iris, "Jim"))
        testthatsomemore::assert(make_piece2()$run(iris, "Jim", "Hester"))
      })

      test_that("it captures the expected partial values during train", {
        expect_contains(make_piece2()$run(iris, "Jim"),
                        list(train = TRUE, first = "Jim", dots = list("Colbert")))
      })

      test_that("it captures the expected full values during train", {
        expect_contains(make_piece2()$run(iris, "Jim", "Hester"),
                        list(train = TRUE, first = "Jim", dots = list("Hester")))
      })

      test_that("it captures partial expressions during train", {
        x <- "Ji"
        expect_contains(make_piece2()$run(iris, paste0(x, "m")),
                        list(train = TRUE, first = "Jim", dots = list("Colbert"),
                             first_expr = quote(paste0(x, "m"))))
      })

      test_that("it captures full expressions during train", {
        x <- "Ji"
        expect_contains(make_piece2()$run(iris, paste0(x, "m"), identity("Hester")),
                        list(train = TRUE, first = "Jim", dots = list("Hester"),
                             first_expr = quote(paste0(x, "m")),
                             dots_expr = list(quote(identity("Hester")))))
      })
    })

    describe("during prediction", {
      test_that("it can predict with unnamed variable arguments", {
        piece <- make_piece2()
        piece$run(iris)
        testthatsomemore::assert(piece$run(iris, "Jim"))
        piece <- make_piece2()
        piece$run(iris)
        testthatsomemore::assert(piece$run(iris, "Jim", "Hester"))
      })

      test_that("it captures the expected partial values during predict", {
        piece <- make_piece2()
        piece$run(iris)
        expect_contains(piece$run(iris, "Jim"),
                        list(train = FALSE, first = "Jim", dots = list("Stewart")))
      })

      test_that("it captures the expected full values during predict", {
        piece <- make_piece2()
        piece$run(iris)
        expect_contains(piece$run(iris, "Jim", "Hester"),
                        list(train = FALSE, first = "Jim", dots = list("Hester")))
      })

      test_that("it captures partial expressions during predict", {
        piece <- make_piece2()
        piece$run(iris)
        x <- "Ji"
        expect_contains(piece$run(iris, paste0(x, "m")),
                        list(train = FALSE, first = "Jim", dots = list("Stewart"),
                             first_expr = quote(paste0(x, "m"))))
      })

      test_that("it captures full expressions during predict", {
        piece <- make_piece2()
        piece$run(iris)
        x <- "Ji"
        expect_contains(piece$run(iris, paste0(x, "m"), identity("Hester")),
                        list(train = FALSE, first = "Jim", dots = list("Hester"),
                             first_expr = quote(paste0(x, "m")),
                             dots_expr = list(quote(identity("Hester")))))
      })
    })
  })
})

describe("with named default arguments", {
  make_piece2 <- function() {
    make_piece(list(first = "Stephen", "Colbert"), list("Jon", first = "Stewart"))
  }

  test_that("it can create a mungepiece without error", {
    testthatsomemore::assert(make_piece2())
  })

  describe("without arguments", {
    test_that("it can train a mungepiece without error", {
      testthatsomemore::assert(make_piece2()$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris))
    })

    test_that("it can predict on a mungepiece without error", {
      piece <- make_piece2()
      piece$run(iris)
      expect_true(piece$mungebit()$trained())
    })
  })

  describe("with unnamed variable arguments", {
    describe("during training", {
      test_that("it can train with unnamed variable arguments", {
        testthatsomemore::assert(make_piece2()$run(iris, "Jim"))
        testthatsomemore::assert(make_piece2()$run(iris, "Jim", "Hester"))
      })

      test_that("it captures the expected partial values during train", {
        expect_contains(make_piece2()$run(iris, "Jim"),
                        list(train = TRUE, first = "Jim", dots = list("Colbert")))
      })

      test_that("it captures the expected full values during train", {
        expect_contains(make_piece2()$run(iris, "Jim", "Hester"),
                        list(train = TRUE, first = "Jim", dots = list("Hester")))
      })

      test_that("it captures partial expressions during train", {
        x <- "Ji"
        expect_contains(make_piece2()$run(iris, paste0(x, "m")),
                        list(train = TRUE, first = "Jim", dots = list("Colbert"),
                             first_expr = quote(paste0(x, "m"))))
      })

      test_that("it captures full expressions during train", {
        x <- "Ji"
        expect_contains(make_piece2()$run(iris, paste0(x, "m"), identity("Hester")),
                        list(train = TRUE, first = "Jim", dots = list("Hester"),
                             first_expr = quote(paste0(x, "m")),
                             dots_expr = list(quote(identity("Hester")))))
      })
    })

    describe("during prediction", {
      test_that("it can predict with unnamed variable arguments", {
        piece <- make_piece2()
        piece$run(iris)
        testthatsomemore::assert(piece$run(iris, "Jim"))
        piece <- make_piece2()
        piece$run(iris)
        testthatsomemore::assert(piece$run(iris, "Jim", "Hester"))
      })

      test_that("it captures the expected partial values during predict", {
        piece <- make_piece2()
        piece$run(iris)
        expect_contains(piece$run(iris, "Jim"),
                        list(train = FALSE, first = "Jim", dots = list("Jon")))
      })

      test_that("it captures the expected full values during predict", {
        piece <- make_piece2()
        piece$run(iris)
        expect_contains(piece$run(iris, "Jim", "Hester"),
                        list(train = FALSE, first = "Jim", dots = list("Hester")))
      })

      test_that("it captures partial expressions during predict", {
        piece <- make_piece2()
        piece$run(iris)
        x <- "Ji"
        expect_contains(piece$run(iris, paste0(x, "m")),
                        list(train = FALSE, first = "Jim", dots = list("Jon"),
                             first_expr = quote(paste0(x, "m"))))
      })

      test_that("it captures full expressions during predict", {
        piece <- make_piece2()
        piece$run(iris)
        x <- "Ji"
        expect_contains(piece$run(iris, paste0(x, "m"), identity("Hester")),
                        list(train = FALSE, first = "Jim", dots = list("Hester"),
                             first_expr = quote(paste0(x, "m")),
                             dots_expr = list(quote(identity("Hester")))))
      })
    })
  })
})

describe("with unnamed default arguments and named argument calls", {
  make_piece2 <- function() {
    make_piece(list("Stephen", "Colbert"), list("Jon", "Stewart"))
  }

  describe("during training", {
    test_that("it can train with unnamed variable arguments", {
      testthatsomemore::assert(make_piece2()$run(iris, first = "Jim"))
      testthatsomemore::assert(make_piece2()$run(iris, first = "Jim", "Hester"))
      testthatsomemore::assert(make_piece2()$run(iris, "Jim", first = "Hester"))
    })

    test_that("it captures the expected partial values during train", {
      expect_contains(make_piece2()$run(iris, first = "Jim"),
                      list(train = TRUE, first = "Jim", dots = list("Colbert")))
    })

    test_that("it captures the expected full values during train", {
      expect_contains(make_piece2()$run(iris, first = "Jim", "Hester"),
                      list(train = TRUE, first = "Jim", dots = list("Hester")))
      expect_contains(make_piece2()$run(iris, "Jim", first = "Hester"),
                      list(train = TRUE, first = "Hester", dots = list("Jim")))
    })

    test_that("it captures partial expressions during train", {
      x <- "Ji"
      expect_contains(make_piece2()$run(iris, first = paste0(x, "m")),
                      list(train = TRUE, first = "Jim", dots = list("Colbert"),
                           first_expr = quote(paste0(x, "m"))))
    })

    test_that("it captures full expressions during train", {
      x <- "Ji"
      expect_contains(make_piece2()$run(iris, first = paste0(x, "m"), identity("Hester")),
                      list(train = TRUE, first = "Jim", dots = list("Hester"),
                           first_expr = quote(paste0(x, "m")),
                           dots_expr = list(quote(identity("Hester")))))
      expect_contains(make_piece2()$run(iris, paste0(x, "m"), first = identity("Hester")),
                      list(train = TRUE, first = "Hester", dots = list("Jim"),
                           first_expr = quote(identity("Hester")),
                           dots_expr = list(quote(paste0(x, "m")))))
    })
  })

  describe("during prediction", {
    test_that("it can predict with unnamed variable arguments", {
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, first = "Jim"))
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, first = "Jim", "Hester"))
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, "Jim", first = "Hester"))
    })

    test_that("it captures the expected partial values during predict", {
      piece <- make_piece2()
      piece$run(iris)
      expect_contains(piece$run(iris, first = "Jim"),
                      list(train = FALSE, first = "Jim", dots = list("Stewart")))
    })

    test_that("it captures the expected full values during predict", {
      piece <- make_piece2()
      piece$run(iris)
      expect_contains(piece$run(iris, first = "Jim", "Hester"),
                      list(train = FALSE, first = "Jim", dots = list("Hester")))
      expect_contains(piece$run(iris, "Jim", first = "Hester"),
                      list(train = FALSE, first = "Hester", dots = list("Jim")))
    })

    test_that("it captures partial expressions during predict", {
      piece <- make_piece2()
      piece$run(iris)
      x <- "Ji"
      expect_contains(piece$run(iris, first = paste0(x, "m")),
                      list(train = FALSE, first = "Jim", dots = list("Stewart"),
                           first_expr = quote(paste0(x, "m"))))
    })

    test_that("it captures full expressions during predict", {
      piece <- make_piece2()
      piece$run(iris)
      x <- "Ji"
      expect_contains(piece$run(iris, first = paste0(x, "m"), identity("Hester")),
                      list(train = FALSE, first = "Jim", dots = list("Hester"),
                           first_expr = quote(paste0(x, "m")),
                           dots_expr = list(quote(identity("Hester")))))
      expect_contains(piece$run(iris, paste0(x, "m"), first = identity("Hester")),
                      list(train = FALSE, first = "Hester", dots = list("Jim"),
                           first_expr = quote(identity("Hester")),
                           dots_expr = list(quote(paste0(x, "m")))))
    })
  })
})

describe("with named default arguments and named argument calls", {
  make_piece2 <- function() {
    make_piece(list(first = "Stephen", "Colbert"), list("Jon", first = "Stewart"))
  }

  describe("during training", {
    test_that("it can train with unnamed variable arguments", {
      testthatsomemore::assert(make_piece2()$run(iris, first = "Jim"))
      testthatsomemore::assert(make_piece2()$run(iris, first = "Jim", "Hester"))
      testthatsomemore::assert(make_piece2()$run(iris, "Jim", first = "Hester"))
    })

    test_that("it captures the expected partial values during train", {
      expect_contains(make_piece2()$run(iris, first = "Jim"),
                      list(train = TRUE, first = "Jim", dots = list("Colbert")))
    })

    test_that("it captures the expected full values during train", {
      expect_contains(make_piece2()$run(iris, first = "Jim", "Hester"),
                      list(train = TRUE, first = "Jim", dots = list("Hester")))
      expect_contains(make_piece2()$run(iris, "Jim", first = "Hester"),
                      list(train = TRUE, first = "Hester", dots = list("Jim")))
    })

    test_that("it captures partial expressions during train", {
      x <- "Ji"
      expect_contains(make_piece2()$run(iris, first = paste0(x, "m")),
                      list(train = TRUE, first = "Jim", dots = list("Colbert"),
                           first_expr = quote(paste0(x, "m"))))
    })

    test_that("it captures full expressions during train", {
      x <- "Ji"
      expect_contains(make_piece2()$run(iris, first = paste0(x, "m"), identity("Hester")),
                      list(train = TRUE, first = "Jim", dots = list("Hester"),
                           first_expr = quote(paste0(x, "m")),
                           dots_expr = list(quote(identity("Hester")))))
      expect_contains(make_piece2()$run(iris, paste0(x, "m"), first = identity("Hester")),
                      list(train = TRUE, first = "Hester", dots = list("Jim"),
                           first_expr = quote(identity("Hester")),
                           dots_expr = list(quote(paste0(x, "m")))))
    })
  })

  describe("during prediction", {
    test_that("it can predict with unnamed variable arguments", {
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, first = "Jim"))
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, first = "Jim", "Hester"))
      piece <- make_piece2()
      piece$run(iris)
      testthatsomemore::assert(piece$run(iris, "Jim", first = "Hester"))
    })

    test_that("it captures the expected partial values during predict", {
      piece <- make_piece2()
      piece$run(iris)
      expect_contains(piece$run(iris, first = "Jim"),
                      list(train = FALSE, first = "Jim", dots = list("Jon")))
    })

    test_that("it captures the expected full values during predict", {
      piece <- make_piece2()
      piece$run(iris)
      expect_contains(piece$run(iris, first = "Jim", "Hester"),
                      list(train = FALSE, first = "Jim", dots = list("Hester")))
      expect_contains(piece$run(iris, "Jim", first = "Hester"),
                      list(train = FALSE, first = "Hester", dots = list("Jim")))
    })

    test_that("it captures partial expressions during predict", {
      piece <- make_piece2()
      piece$run(iris)
      x <- "Ji"
      expect_contains(piece$run(iris, first = paste0(x, "m")),
                      list(train = FALSE, first = "Jim", dots = list("Jon"),
                           first_expr = quote(paste0(x, "m"))))
    })

    test_that("it captures full expressions during predict", {
      piece <- make_piece2()
      piece$run(iris)
      x <- "Ji"
      expect_contains(piece$run(iris, first = paste0(x, "m"), identity("Hester")),
                      list(train = FALSE, first = "Jim", dots = list("Hester"),
                           first_expr = quote(paste0(x, "m")),
                           dots_expr = list(quote(identity("Hester")))))
      expect_contains(piece$run(iris, paste0(x, "m"), first = identity("Hester")),
                      list(train = FALSE, first = "Hester", dots = list("Jim"),
                           first_expr = quote(identity("Hester")),
                           dots_expr = list(quote(paste0(x, "m")))))
    })
  })
})

