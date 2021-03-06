context("munge")

describe("Invalid inputs", {
  test_that("the munge list must be a list", {
    expect_error(munge(iris, 5), "must be a list")
    expect_error(munge(iris, identity), "must be a list")
    expect_error(munge(iris, list2env(list(a = 1))), "must be a list")
  })

  test_that("when munging against a data.frame it must have a mungepieces attribute", {
    expect_error(munge(iris, beaver2), "must have a ")
  })
})

test_that("it does nothing when no mungepieces are passed", {
  expect_equal(munge(iris, list()), iris[,])
})

test_that("it correctly adds to the mungepieces list", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1, 1))
  iris2 <- munge(iris, args)
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("it works with tundraContainers", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1, 1))
  iris2 <- munge(iris, args)
  iris2 <- munge(iris, structure(list(munge_procedure = attr(iris2, "mungepieces")),
                                 class = "tundraContainer"))
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("munge works with environments", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1, 1))
  env <- list2env(list(data = iris))
  iris2 <- munge(env, args)
  expect_equal(length(attr(env$data, 'mungepieces')), 2)
})

test_that("munge can return a stagerunner", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1, 1))
  expect_is(munge(iris, args, stagerunner = TRUE), "stageRunner")
})

test_that("munge can return a stagerunner with remember", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x, one) x + one), 1, 1))
  expect_is(munge(iris, args, stagerunner = list(remember = TRUE)), "stageRunner")
  expect_true(munge(iris, args, stagerunner = list(remember = TRUE))$remember)
})

test_that("it handles a mutating column transformation", {
  args <- lapply(seq_len(2),
    function(.) list(list(column_transformation(function(x, one) {
      input$one <- one; x + one
    }), column_transformation(function(x, ...) { x + input$one })), 1, 1))
  iris2 <- munge(iris, args)
  iris3 <- munge(iris, iris2)
  expect_equal(iris2[TRUE], iris3[TRUE])
})

test_that("it correctly munges a simple mungepiece sequence", {
  args <- lapply(seq_len(2) + 1,
    function(x) list(function(x, v) { x[[1]] <- v * x[[1]]; x }, x))
  iris2 <- munge(iris, args)
  expect_equal(iris2[[1]], iris[[1]] * 6)
})

describe("it can procure the mungepieces list", {
 test_that("it can return the mungelist", {
    args <- lapply(seq_len(2) + 1,
      function(x) list(function(x, v) { x[[1]] <- v * x[[1]]; x }, x))
    list <- munge(iris, args, list = TRUE)
    expect_equal(munge(iris, args), munge(iris, list))
  })
})

describe("adding names to the mungepieces attribute", {
  test_that("it respects when names are given with a stagerunner munger", {
    env <- list2env(list(data = iris))
    rnr <- munge(env, list(foo = list(function(x, y) { x[y, ]}, 1:100),
                           bar = list(function(y) { y[[1]] <- y[[1]] * 2; y })),
                 stagerunner = list(remember = TRUE))
    rnr$run()
    expect_equal(names(attr(env$data, "mungepieces")), c("foo", "bar"))
  })

  test_that("it respects when names are given", {
    out <- munge(iris, list(foo = list(function(x, y) { x[y, ]}, 1:100),
                            bar = list(function(y) { y[[1]] <- y[[1]] * 2; y })))
    expect_equal(names(attr(out, "mungepieces")), c("foo", "bar"))
  })

  test_that("it respects when no names are given", {
    out <- munge(iris, list(list(function(x, y) { x[y, ]}, 1:100),
                            list(function(y) { y[[1]] <- y[[1]] * 2; y })))
    expect_equal(names(attr(out, "mungepieces")), NULL)
  })

  test_that("it respects when partial names are given", {
    out <- munge(iris, list(list(function(x, y) { x[y, ]}, 1:100),
                            bar = list(function(y) { y[[1]] <- y[[1]] * 2; y })))
    expect_equal(names(attr(out, "mungepieces")), c(NA_character_, "bar"))
  })
})

describe("using mungepieces with inputs", {

  simple_imputer <- function(...) {
    imputer_train <- imputer_predict <- function(data, col) {
      if (!isTRUE(trained)) {
        input$mean <- mean(data[[col]], na.rm = TRUE)
      }

      data[[col]][is.na(data[[col]])] <- input$mean
      data
    }
    mungepiece$new(mungebit$new(imputer_train), list(...))
  }

  drop_variables <- function(data, variables) {
    data[variables] <- vector("list", length(variables))
    data
  }

  simple_discretize <- function(data, variable, cutoff) {
    input$cutoff <- cutoff
    data[[variable]] <- factor(ifelse(data[[variable]] > cutoff, "Right", "Left"),
                               levels = c("Left", "Right"))
    data
  }

  simple_restore_levels <- function(data, variable, ...) {
    data[[variable]] <- factor(ifelse(data[[variable]] > input$cutoff, "Right", "Left"),
                               levels = c("Left", "Right"))
    data
  }

  test_that("it munges a sequence of mungepieces with inputs", {
    imputer    <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    
    iris2 <- munge(iris, list("Impute first column" = imputer, 
                              "Drop species"        = list(drop_variables, "Species")))
    expect_equal(iris2[1, 1], mean(iris$Sepal.Length[-1]))
  })

  test_that("it munges in predict mode a sequence of mungepieces with inputs", {
    imputer    <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    
    iris2 <- munge(iris, list("Impute first column" = imputer, 
                              "Drop species"        = list(drop_variables, "Species")))
    iris3 <- munge(iris, iris2)
    expect_equal(iris3, iris2)
  })

  test_that("it munges in conjunction with separate train and predict args", {
    imputer    <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    irisprime  <- transform(iris, dependent_variable = c(0L, 1L))
    
    iris2 <- munge(irisprime, list(
      "Select training var" = list(train = list(`[`, colnames(irisprime)),
                                   predict = list(`[`, colnames(iris))),
      "Impute first column" = imputer, 
      "Drop species"        = list(drop_variables, "Species")
    ))

    iris3 <- munge(iris, iris2)
    # Use TRUE to induce a copy and drop attributes.
    expect_equal(iris3[TRUE], iris2[colnames(iris3)]) 
  })

  test_that("it munges in conjunction with separate train and predict functions", {
    imputer    <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    irisprime  <- transform(iris, dependent_variable = c(0L, 1L))
    
    iris2 <- munge(irisprime, list(
      "Select training var" = list(train = list(`[`, colnames(irisprime)),
                                   predict = list(`[`, colnames(iris))),
      "Impute first column" = imputer, 
      "Dumb discretize"     = list(list(simple_discretize, simple_restore_levels),
                                   "Sepal.Width", 3),
      "Drop species"        = list(drop_variables, "Species")
    ))

    iris3 <- munge(iris, iris2)
    # Use TRUE to induce a copy and drop attributes.
    expect_equal(iris3[TRUE], iris2[colnames(iris3)]) 
  })

  test_that("it removes NULL values from the munge procedure", {
    imputer    <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    
    iris2 <- munge(iris, list("Impute first column" = imputer, 
                              "Foo"                 = NULL,
                              "Drop species"        = list(drop_variables, "Species")))
    expect_equal(iris2[1, 1], mean(iris$Sepal.Length[-1]))
  })

  describe("One-sided munging", {
    test_that("it works with a predict-only mungebit", {
      iris2 <- munge(iris, list("Drop nothing" = list(list(NULL, drop_variables), "Species")))
      expect_equal(iris2[TRUE], iris)
      expect_equal(munge(iris, iris2)[TRUE], iris[1:4])
    })

    test_that("it works with a train-only mungebit", {
      iris2 <- munge(iris, list("Drop nothing" = list(list(drop_variables, NULL), "Species")))
      expect_equal(iris2[TRUE], iris[1:4])
      iris3 <- munge(iris, iris2)
      expect_equal(iris3[TRUE], iris)
    })
  })

  describe("Legacy mungebits", {
    legacy_identity_function <- function(dataframe, ...) { dataframe }
    class(legacy_identity_function) <- c("legacy_mungebit_function", "transformation", "function")

    iris2 <- munge(iris, list("Apply legacy identity" = list(list(legacy_identity_function, NULL))))
    test_that("it attaches legacy mungebits, with names, to the dataframe", {
      expect_equal(names(attr(iris2, "mungepieces")), "Apply legacy identity")
    })
  })

})

