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
})

