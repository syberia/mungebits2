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
    function(.) list(column_transformation(function(x, one) x + one), 1))
  iris2 <- munge(iris, args)
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("it correctly munges a simple mungepiece sequence", {
  args <- lapply(seq_len(2) + 1,
    function(x) list(function(x, v) { x[[1]] <- v * x[[1]]; x }, x))
  iris2 <- munge(iris, args)
  expect_equal(iris2[[1]], iris[[1]] * 6)
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

  test_that("it munges a sequence of mungepieces with inputs", {
    imputer <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    
    iris2 <- munge(iris, list("Impute first column" = imputer, 
                              "Drop species"        = list(drop_variables, "Species")))
    expect_equal(iris2[1, 1], mean(iris$Sepal.Length[-1]))
  })

  test_that("it munges in predict mode a sequence of mungepieces with inputs", {
    imputer <- simple_imputer("Sepal.Length")
    iris[1, 1] <- NA
    
    iris2 <- munge(iris, list("Impute first column" = imputer, 
                              "Drop species"        = list(drop_variables, "Species")))
    iris3 <- munge(iris, iris2)
    expect_equal(iris3, iris2)
  })

})

