# TODO: (RK) Add notice if mungebits and mungebits2 are
# loaded at the same time.

.onLoad <- function(libPath, pkg) {
  if (as.package_version(R.version) < as.package_version("3.1.0")) {
    packageStartupMessage(crayon::red(paste0(
      "Using mungebits2 with R version < 3.1 will result ",
      "in dramatic performance slowdowns."
    )))
  }
}

globalVariables(c("newpieces", "mungepieces", "size"))

