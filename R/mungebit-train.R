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
#' @return The modified \code{data}, whether it is an \code{environment}
#'   or \code{data.frame}. Side effects on the \code{input} local variable
#'   provided to the \code{train_function} will be recorded on the mungebit
#'   object.
mungebit_train <- function(data, ...) {
  if (enforce_train) {
    on.exit(self$.trained <- TRUE)
  }

  ## The `input` environment used by the mungebit to record metadata from
  ## the munging performed at training-time is the only opportunity for
  ## affecting the `input` environment. Afterwards, we [lock it](https://stat.ethz.ch/R-manual/R-devel/library/base/html/bindenv.html)
  ## so that we are confident the user does not modify it during prediction
  ## time (i.e., when it is run in a real-time production system).
  on.exit(lockEnvironment(self$.input), add = TRUE)

  ## We inject the `input` helper so that the mungebit
  ## can remember necessary metadata for replicating the
  ## munging operation at prediction time.
  inject_metadata(self.$train_function, self$.input, self$.trained)(data, ...)
}

inject_metadata <- function(func, input, trained) {
  ## If there is no training or prediction function, we perform 
  ## *no transformation* on the data or the mungebit `input`, i.e.,
  ## we use the [`identity` function](https://stat.ethz.ch/R-manual/R-devel/library/base/html/identity.html).
  if (is.null(func)) {
    identity
  } else {
    copy       <- func
    debug_flag <- isdebugged(func)

    environment(copy) <- list2env(list(
      input   = input,
      ## We also inject a helper called `trained` used for discriminating
      ## whether the function has been trained already.
      trained = isTRUE(trained)
    ), parent = environment(func))

    ## Touching a function's environment like in the expression above
    ## *clears its internal debug flag*. We restore the flag to indicate
    ## it is being debugged. I don't know how to detect whether a function
    ## is [`debugonce`d](https://stat.ethz.ch/R-manual/R-devel/library/base/html/debug.html)
    ## so if you know how to restore this flag please submit a
    ## [pull request](https://github.com/robertzk/mungebits2).
    if (isdebugged(func)) {
      debug(copy)
    }

    copy
  }
}

