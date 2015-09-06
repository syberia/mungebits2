## Imagine running an imputation script on a dataset. 
## On a training set, we have to compute the mean and replace
## the `NA`s with its value. However, when a single row comes
## in through a streaming production system, we merely need to
## memorize the computed mean and replace a variable with it
## if it is `NA`.
##
## Calling the `run` method on a mungebit will store any
## data it needs for production, such as imputed means,
## in the `input` member. The second time `$run` is called
## (i.e., during prediction or real-time production use),
## it will be using the `predict_function` rather than the
## `train_function`, which will be less computationally expensive
## since it does not have to operate in reference to a training set
## and can use the memorized results in `input` to achieve
## the same transformation as the `train_function`.
#' Run a mungebit.
#' 
#' Imagine flipping a switch on a set of train tracks. A mungebit
#' behaves like this: once the \code{trained} switch is flipped,
#' it can only run the \code{predict_function}, otherwise it will
#' run the \code{train_function}.
#'
#' @rdname mungebit
#' @param data environment or data.frame. Essentially an environment
#'   containing a \code{data} variable. In this case, that \code{data} variable
#'   will have a side effect enacted on it. If a \code{data.frame}, then 
#'   the return value will be the modified \code{data.frame} and the mungebit
#'   will record any results it must memorize in its \code{input}.
#' @param ... additional arguments to the mungebit's \code{train_function} or
#'   \code{predict_function}.
#' @return The modified \code{data}, whether it is an \code{environment}
#'   or \code{data.frame}.
mungebit_run <- function(data, ...) {
  if (is.environment(data)) {
    if (!exists("data", envir = data, inherits = FALSE)) {
      stop("If you are passing an environment to a mungebit, you must ",
           "provide one that contains a ", sQuote("data"), " key.")
    }

    data$data <- self$run(data$data, ...)
  } else if (isTRUE(self$.trained)) {
    data <- self$predict(data, ...)
  } else {
    data <- self$train(data, ...)  
  }
  data
}

