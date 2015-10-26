context("utils")

describe("list2env_safe", {
  test_that("it works with 0 element lists", {
    expect_equal(length(list2env_safe(list())), 0) 
  })

  test_that("it works with 2 element lists", {
    expect_equal(length(list2env_safe(list(a = 1, b = 2))), 2) 
  })
})

