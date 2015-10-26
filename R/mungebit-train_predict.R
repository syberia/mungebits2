#' Run the train function on a mungebit.
#'
#' The train function is responsible for performing a munging step and
#' storing metadata that can replicate the munging step in a live
#' production environment without the full reference data set.
#'
#' The purpose of the train function is to
#'
#' \enumerate{
#'   \item{Perform some munging on the data set, such as renaming
#'     columns, creating derived features, performing principal component
#'     analysis, replacing some values, removing outliers, etc.}
#'   \item{Storing the metadata necessary to replicate the munging operation
#'     after the original training set is no longer available. For example,
#'     if we are imputing a variable, we would need to remember its mean
#'     so we can use it later to replace \code{NA} values.}
#' }
#'
#' @rdname mungebit
#' @inheritParams mungebit_run
#' @param _envir environment. Internal argument used for determining
#'   the execution context of the invoked \code{train_function} or
#'   \code{predict_function}.
#' @return The modified \code{data}, whether it is an \code{environment}
#'   or \code{data.frame}. Side effects on the \code{input} local variable
#'   provided to the \code{train_function} will be recorded on the mungebit
#'   object.
mungebit_train <- function(data, ..., `_envir` = parent.frame()) {
  if (self$.enforce_train) {
    if (isTRUE(self$.trained)) {
      stop("This mungebit has already been trained, cannot re-train.")
    }
    on.exit(self$.trained <- TRUE, add = TRUE)
  }

  ## The `input` environment used by the mungebit to record metadata from
  ## the munging performed at training-time is the only opportunity for
  ## affecting the `input` environment. Afterwards, we [lock it](https://stat.ethz.ch/R-manual/R-devel/library/base/html/bindenv.html)
  ## so that we are confident the user does not modify it during prediction
  ## time (i.e., when it is run in a real-time production system).
  if (isTRUE(self$.enforce_train)) {
    on.exit({
      lockEnvironment(self$.input, TRUE)
      if (!is.null(self$.predict_function)) {
        environment(self$.predict_function)$trained <- TRUE
      }
    }, add = TRUE)
  }

  if (is.null(self$.train_function)) {
    data
  } else if (self$.nse) {
    args <- c(list(substitute(data)), eval(substitute(alist(...))))
    do.call(self$.train_function, args, envir = `_envir`)
  } else {
    self$.train_function(data, ...)
  }
}

#' Run the predict function on a mungebit.
#'
#' The predict function is responsible for performing a munging step
#' using metadata it computed during an earlier training step.
#' This is usually done in a live production environment setting.
#'
#' The purpose of the predict function is to
#'
#' \enumerate{
#'   \item{Perform some munging on the data set, such as renaming
#'     columns, creating derived features, performing principal component
#'     analysis, replacing some values, removing outliers, etc.}
#'   \item{Use the metadata computed during the \code{train} step
#'    to correctly perform this munging.}
#' }
#'
#' @rdname mungebit
#' @inheritParams mungebit_run
#' @return The modified \code{data}, whether it is an \code{environment}
#'   or \code{data.frame}. Side effects on the \code{input} local variable
#'   provided to the \code{predict_function} will be recorded on the mungebit
#'   object.
mungebit_predict <- function(data, ..., `_envir` = parent.frame()) {
  # For some reason, accessing this takes some time..
  if (!isTRUE(self$.trained)) {
    stop("This mungebit cannot predict because it has not been trained.")
  }

  if (self$.nse) {
    args <- c(list(substitute(data)), eval(substitute(alist(...))))
    do.call(self$.predict_function, args, envir = `_envir`)
  } else {
    self$.predict_function(data, ...)
  }
}

