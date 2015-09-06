context("inject_metadata")
library(testthatsomemore)

test_that("it does not error", {
  testthatsomemore::assert(inject_metadata(identity, list(), TRUE))          
})


