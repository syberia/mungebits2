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
#'   or \code{data.frame}. Side effects on the \code{inputs} local variable
#'   provided to the \code{train_function} will be recorded on the mungebit
#'   object.
mungebit_train <- function(data, ...) {
  if (!is.null(train_function)) {
    original_env <- environment(train_function)
    inject_inputs(train_function)
    on.exit(environment(train_function) <<- original_env)

    train_function(mungeplane$data, ...) 

    # TODO: Oh no. :( Sometimes inputs is being set and sometimes
    # environment(train_function)$inputs is being set--I think this
    # has to do with changing the environment of the function that's
    # running. How do we get around this? This seems incredibly messy.
    inputs <<-
      if (length(tmp <- environment(train_function)$inputs) > 0) tmp
      else inputs
  }
  if (enforce_train) trained <<- TRUE
  invisible(TRUE)
}
