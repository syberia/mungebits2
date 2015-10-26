context("printing")

describe("printing mungepieces", {
  test_that("it can print a simple mungepiece", {
    expect_output(print(mungepiece$new(mungebit$new())), "train and predict")
    expect_output(print(mungepiece$new(mungebit$new())), "untrained")
  })

  test_that("it can display full function body", {
    mp <- mungepiece$new(mungebit$new(function(x) { a; b; c; d; e; f; g; h; boom }))
    out <- capture.output(print(mp))
    expect_false(any(grepl("boom", out)))
    expect_output(print(mp, full = TRUE), "boom")
  })
  
  test_that("it can display full train_args", {
    mp <- mungepiece$new(mungebit$new(), list(body(utils::install.packages)))
    out <- capture.output(print(mp))
    expect_false(any(grepl("boom", out)))
    expect_output(print(mp, full = TRUE), "destdir")
  })
  
  test_that("it can display full predict_args", {
    mp <- mungepiece$new(mungebit$new(), list(body(utils::install.packages)))
    out <- capture.output(print(mp))
    expect_false(any(grepl("boom", out)))
    expect_output(print(mp, full = TRUE), "destdir")
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
  
  test_that("it can display full function body", {
    mb <- mungebit$new(function(x) { a; b; c; d; e; f; g; h; boom })
    out <- capture.output(print(mb))
    expect_false(any(grepl("boom", out)))
    expect_output(print(mb, full = TRUE), "boom")
  })
  
  test_that("it can display full inputs", {
    mb <- mungebit$new(function(x) { input$foo <- body(utils::install.packages); x })
    out <- capture.output(print(mb))
    expect_false(any(grepl("destdir", out)))
    mb$run(iris)
    expect_output(print(mb, full = TRUE), "destdir")
  })
  
  test_that("it can explain a mungebit uses nonstandard evaluation", {
    mb <- mungebit$new(nse = TRUE)
    expect_output(print(mb), "nonstandard")
  })

  test_that("it train and predict args when they are identical", {
    mp  <- mungepiece$new(mungebit$new(), list(a = 1))
    expect_output(print(mp), "train and predict arguments")
  })

  test_that("it hides predict args when none are present", {
    mp  <- mungepiece$new(mungebit$new(), train_args = list(a = 1), predict_args = list())
    out <- capture.output(print(mp))
    expect_false(any(grepl("predict arguments", out)))
    expect_output(print(mp), "train arguments")
  })

  test_that("it hides train args when none are present", {
    mp  <- mungepiece$new(mungebit$new(), train_args = list(), predict_args = list(a = 1))
    out <- capture.output(print(mp))
    expect_false(any(grepl("train arguments", out)))
    expect_output(print(mp), "predict arguments")
  })
})

describe("transformations", {
  test_that("it can print a column transformation", {
    expect_output(print(column_transformation(identity)), "Column transformation")
  })

  test_that("it can print a column transformation", {
    expect_output(print(column_transformation(identity)), "Column transformation")
  })
  
  test_that("it can print a multi column transformation", {
    expect_output(print(multi_column_transformation(identity)), "Multi column transformation")
  })
  
  test_that("it can display full column transformation body", {
    ct <- column_transformation(utils::install.packages)
    out <- capture.output(print(ct))
    expect_false(any(grepl("Updating HTML", out)))
    expect_output(print(ct, full = TRUE), "Updating HTML")
  })
  
  test_that("it squishes braces", {
    ct <- column_transformation(function(x) { 2 * x + 1 })
    expect_output(print(ct), "function (x) {\n    2 * x", fixed = TRUE)
  })
  
  test_that("it informs us when using nonstandard evaluation", {
    ct <- column_transformation(function(x) { 2 * x + 1 }, nonstandard = TRUE)
    expect_output(print(ct), "non-standard", fixed = TRUE)
  })
  
  test_that("it informs us when using nonstandard evaluation for multi column transformations", {
    ct <- multi_column_transformation(function(x) { 2 * x + 1 }, nonstandard = TRUE)
    expect_output(print(ct), "non-standard", fixed = TRUE)
  })
})

