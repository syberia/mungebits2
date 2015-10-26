## In R 3.1, calling a function like
##
## ```r
## function(data) {
##   data[[1]] <- 2 * data[[1]]
##   data
## }
## ```
## 
## will create a copy of the first column of `data`. Before R 3.1,
## it will create a copy of **the entire dataset**, even if it has thousands
## of other columns. This affects the performance of train and predict
## functions in `mungebit` objects. There is a workaround in the mungebits
## package at the expense of using non-standard evaluation and making
## every train and predict function look like
##
## ```r
## function(data) {
##   eval.parent(substitute({ # Evaluating in the calling environment
##                            # prevents creation of a copy.
##     data[[1]] <- 2 * data[[1]]
##     data
##   })
## }
## ```
##
## Gross!
.onLoad <- function(libPath, pkg) {
  if (as.package_version(R.version) < as.package_version("3.1.0")) {
    packageStartupMessage(crayon::red(paste0(
      "Using the mungebits2 package with R version < 3.1 will result ",
      "in dramatic performance slowdowns: use the mungebits package instead ",
      "(https://github.com/robertzk/mungebits)"
    )))
  }
}

## If mungebits is attached after mungebits2, it will *overwrite*
## many functions from mungebits2, since they share the same name.
## This lack of compatible namespacing is a design flaw in R, but
## for now we just alert the user.
##
## This trick was shamelessly borrowed / stolen from
## [dplyr](https://github.com/hadley/dplyr). :)
.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("mungebits", "attach"), function(...) {
    packageStartupMessage(crayon::red$bold(paste0(
      "You have loaded mungebits after mungebits2 - ",
      "this is likely to cause problems.\nIf you need functions from both ",
      "mungebits and mungebits2 (which is unlikely), please load mungebits first, ",
      "then mungebits2:\nlibrary(mungebits); library(mungebits2)")))
  })
}



