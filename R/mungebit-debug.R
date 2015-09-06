#' Generic debugging.
#'
#' @inheritParams base::debug
#' @seealso \code{\link[base]{debug}}
#' @export
debug <- function(fun, text = "", condition = NULL) {
  UseMethod("debug")
}

#' @export
debug.default <- base::debug

#' @export 
debug.mungebit <- function(fun, text = "", condition = NULL) {
  debug(fun$.train_function, text, condition)
  debug(fun$.predict_function, text, condition)
}

#' Generic removal of debugging.
#'
#' @inheritParams base::undebug
#' @seealso \code{\link[base]{undebug}}
#' @export
undebug <- function(fun) {
  UseMethod("undebug")
}

#' @export
undebug.default <- base::undebug

#' @export 
undebug.mungebit <- function(fun) {
  undebug(fun$.train_function)
  undebug(fun$.predict_function)
}

