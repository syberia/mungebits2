context("utils")
library(testthatsomemore)

describe("list2env_safe", {
  test_that("it works with 0 element lists", {
    expect_equal(length(list2env_safe(list())), 0) 
  })

  test_that("it works with 2 element lists", {
    expect_equal(length(list2env_safe(list(a = 1, b = 2))), 2) 
  })
})

describe("zzz", {
  test_that("it can call onLoad", {
    testthatsomemore::package_stub("base", "as.package_version", function(...) {
      if (!is.character(..1)) structure(list(c(3, 0, 0)), class = c("R_system_version", "package_version", "numeric_version"))
      else ..1        
    }, 
    testthatsomemore::package_stub("base", "packageStartupMessage", function(x) {
      expect_true(grepl("with R version", x))
    }, .onLoad()))
  })

  test_that("it can call onAttach", {
    testthatsomemore::package_stub("base", "setHook", function(ev, fn) { fn() }, 
    testthatsomemore::package_stub("base", "packageStartupMessage", function(x) {
      expect_true(grepl("have loaded mungebits", x))
    }, .onAttach()))
  })
})

