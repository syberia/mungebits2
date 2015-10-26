.onLoad <- function(libPath, pkg) {
  if (as.package_version(R.version) < as.package_version("3.1.0")) {
    packageStartupMessage(crayon::red(paste0(
      "Using the mungebits2 package with R version < 3.1 will result ",
      "in dramatic performance slowdowns: use the mungebits package instead ",
      "(https://github.com/robertzk/mungebits)"
    )))
  }
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("mungebits", "attach"), function(...) {
    packageStartupMessage(crayon::red$bold(paste0(
      "You have loaded mungebits after mungebits2 - ",
      "this is likely to cause problems.\nIf you need functions from both ",
      "mungebits and mungebits2 (which is unlikely), please load mungebits first, ",
      "then mungebits2:\nlibrary(mungebits); library(mungebits2)")))
  })
}


globalVariables(c("newpieces", "mungepieces", "size"))

