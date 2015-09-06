## Debugging the train and predict function of a mungebit should be
## transparent to the user. Say we have a mungebit called `value_replacer`.
## By calling `debug(value_replacer)`, we should be able to simultaneously
## set debug hooks on both the `train_function` and `predict_function`
## of the mungebit. Calling `undebug(value_replacer)` will remove the hooks.
##
## R has a tremendous array of debugging tools. You should familiarize
## yourself with them to make your life much simpler. A great resource
## is chapter 8 of [The R Inferno](http://www.burns-stat.com/pages/Tutor/R_inferno.pdf).
#' Generic debugging.
#'
#' @inheritParams base::debug
#' @seealso \code{\link[base]{debug}}
#' @export
debug <- function(fun, text = "", condition = NULL) {
  ## The standard [S3 generic](http://adv-r.had.co.nz/OO-essentials.html).
  UseMethod("debug")
}

## By default, debugging should preserve the behavior from the base package.
#' @export
debug.default <- base::debug

#' @export 
debug.mungebit <- function(fun, text = "", condition = NULL) {
  ## To debug a mungebit, we loop over the train and predict functions
  ## and set their internal debugging flag. The `if` statement is
  ## necessary in case either are `NULL` (e.g., there is no train
  ## or predict step).
  for (fn in list(fun$.train_function, fun$.predict_function)) {
    if (is.function(fn)) {
      debug(fn, text, condition)
    }
  }
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
  ## To undebug a mungebit, we loop over the train and predict functions
  ## and unset their internal debugging flag. The `if` statement is
  ## necessary in case either are `NULL` (e.g., there is no train
  ## or predict step), and to avoid throwing a warning if the function
  ## isn't already being debugged.
  for (fn in list(fun$.train_function, fun$.predict_function)) {
    if (is.function(fn) && isdebugged(fn)) {
      undebug(fn)
    }
  }
}

