context("printing")

describe("printing mungepieces", {
  test_that("it can print a simple mungepiece", {
    expect_output(print(mungepiece$new(mungebit$new())), "train and predict")
    expect_output(print(mungepiece$new(mungebit$new())), "untrained")
  })

})

describe("printing mungebits", {
  test_that("it can print a split mungebit", {
    expect_output(print(mungebit$new(function(x) 2 * x + 1, NULL)), "train function")
    expect_output(print(mungebit$new(function(x) 2 * x + 1, NULL)),
                  "function (x) 2 * x + 1", fixed = TRUE)
  })

  test_that("it can print a split mungebit on predict side", {
    expect_output(print(mungebit$new(NULL, function(x) 2 * x + 1)), "predict function")
    expect_output(print(mungebit$new(NULL, function(x) 2 * x + 1)),
                  "function (x) 2 * x + 1", fixed = TRUE)
  })

  test_that("it can print lack of train function", {
    expect_output(print(mungebit$new(NULL, identity)), "No train")
  })

  test_that("it can print lack of predict function", {
    expect_output(print(mungebit$new(identity, NULL)), "No predict")
  })
  
  test_that("it can display if a mungebit is trained", {
    mb <- mungebit$new()
    expect_output(print(mb), "ntrained")
    mb$run(iris)
    expect_output(print(mb), "[^n]trained")
  })
  
  test_that("it can display if a mungebit is trained", {
    mb <- mungebit$new(function(x) { e <- get(paste0("in","put")); e$true <- TRUE; x })
    out <- capture.output(print(mb))
    expect_false(any(grepl("input", out)))
    mb$run(iris)
    expect_output(print(mb), "input")
  })
})

