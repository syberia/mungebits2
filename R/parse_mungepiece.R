## Constructing a mungepiece is not incredibly straightforward. First,
## we must construct the mungebit, which represents the code that will
## be executed when running the mungepiece on a training dataset to
## later feed to a machine learning classifier (i.e., the train function)
## in conjunction with the code that executes on streaming records
## coming through in a production system performing the same
## mathematical operation on a 1-row dataset (i.e., the predict function).
##
## Next, we must determine the training and prediction arguments to the
## mungebit that specify the difference in how to use the mungebit on
## offline training versus realtime prediction data.
##
## Thus, constructing a mungepiece looks something like:
## 
## ```r
## piece <- mungepiece$new(
##   mungebit$new(train_function, predict_function),
##   train_args, predict_args
## )
## ```
##
## In particular, we have to invoke the `mungebit` constructor every time
## we create a mungepiece. Instead the `parse_mungepiece` helper defined in
## this file provides a more convenient format:
##
## ```r
## # If the train function with train args is the same as the predict function
## # with predict args.
## piece <- parse_mungepiece((list(train_fn, train_arg1, train_arg2 = "blah"))
##
## # If the train and predict arguments to the mungepiece match, but we
## # wish to use a different train versus predict function for the mungebit.
## piece <- parse_mungepiece(list(list(train_fn, predict_fn), dual_arg1, dual_arg2 = "blah"))
## 
## # If we wish to only run this mungepiece during training.
## piece <- parse_mungepiece(list(list(train_fn, NULL), train_arg1, train_arg2 = "blah"))
## 
## # If we wish to only run this mungepiece during prediction
## piece <- parse_mungepiece(list(list(NULL, predict_fn), predict_arg1, predict_arg2 = "blah"))
##
## # If we wish to run different arguments but the same function during
## # training versus prediction.
## piece <- parse_mungepiece(list(train = list(train_fn, train_arg1),
##                                predict = list(train_fn, predict_arg1)))
##
## # If we wish to run different arguments with different functions during
## # training versus prediction.
## piece <- parse_mungepiece(list(train = list(train_fn, train_arg1),
##                                predict = list(predict_fn, predict_arg1)))
## ```
## 
## This is a full partition of the potential arguments used to initialize a
## mungebit + mungepiece combo. Using this syntax in conjunction with the
## `munge` helper function speeds up coding of munge procedures (lists of
## mungepieces) and increases the readability of munging code.
##
## ```r
## # The munge function calls out to the parse_mungepiece helper.
## munged_data <- munge(raw_data, list(
##   "Drop useless vars" = list(list(drop_vars, vector_of_variables),
##                              list(drop_vars, c(vector_variables, "dep_var"))),
##   "Impute variables"  = list(imputer, imputed_vars),
##   "Discretize vars"   = list(list(discretize, restore_levels), discretized_vars)
## ))
## ```
## 
## Translated in English, we are saying:
##
##   1. Drop a static list of useless variables from the data set.
##      When the model is trained, drop the dependent variable as well
##      since we will no longer need it.
##
##   2. Impute the variables in the static list of `imputed_vars`.
##      When the model is trained, the `imputer` will have some logic
##      to restore the means obtained during training of the mungepiece
##      (assuming we are using mean imputation).
##
##   3. Discretize the static list of variables in the `discretized_vars`
##      character vector. After model training, when new data points come in,
##      the original training set is no longer available. The `discretize`
##      method stored the necessary cuts for each variable in the mungebit's
##      `input`, which the `restore_levels` function uses to bin the
##      numerical features in the list of `discretized_vars` into factor
##      (categorical) variables.
##
## If one took an initial (training) data set, ran it through the
## `munge` helper as above, took the resulting list of mungepieces,
## and ran the original data set through them a second time (so they
## are running in "predict mode"), we should obtain the same result.
## 
## We can use the list of trained mungepieces to replicate the munging
## on the training set in a production system on single row data sets
## (i.e., new records being streamed in real-time).
#' Translate a list of train / predict functions and arguments to a mungepiece.
#'
#' Constructing mungepieces and mungebits by hand is a little tedious.
#' To simplify the process, we introduce a tiny DSL that allows for
#' easier construction of mungebits. The intention is for this function
#' to be used in conjuction with a list passed to the \code{\link{munge}}
#' helper.
#'
#' @note To understand the documentation of this helper, please read
#'   the documentation on \code{\link{mungebit}} and \code{\link{mungepiece}}
#'   first.
#' @param args list. A list of arguments. This can be one of the following formats
#'   
#'   \enumerate{
#'     \item{\code{list(train_fn, ...)}}{ -- If the first element of \code{args} is
#'       a function followed by other arguments, the constructed mungepiece
#'       will use the \code{train_fn} as both the \emph{train and predict}
#'       function for the mungebit, and \code{list(...)} (that is, the remaining 
#'       elements in the list) will be used as both the train and predict
#'       arguments in the mungepiece. In other words, using this format
#'       specifies you would like \emph{exactly the same behavior in
#'       training as in prediction}. This is appropriate for mungebits
#'       that operate in place and do not need information obtained
#'       from the training set, such as simple value replacement or
#'       column removal.
#'     }
#'     \item{\code{list(list(train_fn, predict_fn), ...)}}{
#'       -- If \code{args} consists of a two-element pair in its first
#'       element, it must be a pair of either \code{NULL}s or functions,
#'       with not both elements \code{NULL}. If the \code{train_fn}
#'       or \code{predict_fn}, resp., is \code{NULL}, this will signify to have
#'       \emph{no effect} during training or prediction, resp.
#'        
#'       The remaining arguments, that is \code{list(...)}, will be used
#'       as both the training and prediction arguments.
#'   
#'       This structure is ideal if the behavior during training and prediction
#'       has an identical parametrization but very different implementation,
#'       such as imputation, so you can pass two different functions.
#'
#'       It is also useful if you wish to have no effect during prediction,
#'       such as removing faulty rows during training, or no effect during
#'       training, such as making a few transformations that are only
#'       necessary on raw production data rather than the training data.
#'     }
#'     \item{\code{list(train = list(train_fn, ...), predict = list(predict_fn, ...))}}{
#'       If \code{args} consists of a list consisting of exactly two named
#'       elements with names "train" and "predict", then the first format will be
#'       used for the respective fields. In other words, a mungepiece will
#'       be constructed consisting of a mungebit with \code{train_fn} as the
#'       training function, \code{predict_fn} as the predict fuction, and
#'       the mungepiece train arguments will be the train list of additional
#'       arguments \code{list(...)}, and similarly the predict arguments will be
#'       the predict list of additional arguments \code{list(...)}.
#'  
#'       Note \code{train_fn} and \code{predict_fn} must \emph{both} be functions
#'       and not \code{NULL}, since then we could simply use the second format
#'       described above.
#'
#'       This format is ideal when the parametrization differs during training and
#'       prediction. In this case, \code{train_fn} usually should be the same
#'       as \code{predict_fn}, but the additional arguments in each list can
#'       be used to identify the parametrized discrepancies. For example, to
#'       sanitize a dataset one may wish to drop unnecessary variables. During
#'       training, this excludes the dependent variable, but during prediction
#'       we may wish to drop the dependent as well.
#'
#'       This format can also be used to perform totally different behavior on
#'       the dataset during training and prediction (different functions and
#'       parameters), but mungebits should by definition achieve the same
#'       operation during training and prediction, so this use case is rare
#'       and should be handled carefully.
#'     }
#'   }
#'
#'   Note that the above trichotomy is exhaustive: any mungepiece can be
#'   constructed using this helper, regardless of its mungebit's
#'   train or predict function or its own train or predict arguments.
#'   In the first two formats, the first unnamed list element is always
#'   reserved and will never belong to the \code{train_args} or \code{predict_args}
#'   of the mungepiece.
#'
#'   Also note that in the first two formats, the first list element must be
#'   unnamed.
#' @return The constructed \code{\link{mungepiece}}.
#' @seealso \code{\link{mungepiece}}, \code{\link{mungebit}}.
#' @export
#' @examples
#' # First, we show off the various formats that the parse_mungepiece
#' # helper accepts. For this exercise, we can use dummy train and
#' # predict functions and arguments.
#' train_fn   <- predict_fn   <- base::identity
#' train_arg1 <- predict_arg1 <- dual_arg1 <- TRUE # Can be any parameter value.
#'
#' # If the train function with train args is the same as the predict function
#' # with predict args.
#' piece <- parse_mungepiece(list(train_fn, train_arg1, train_arg2 = "blah"))
#'
#' # If the train and predict arguments to the mungepiece match, but we
#' # wish to use a different train versus predict function for the mungebit.
#' piece <- parse_mungepiece(list(list(train_fn, predict_fn), dual_arg1, dual_arg2 = "blah"))
#' 
#' # If we wish to only run this mungepiece during training.
#' piece <- parse_mungepiece(list(list(train_fn, NULL), train_arg1, train_arg2 = "blah"))
#' 
#' # If we wish to only run this mungepiece during prediction
#' piece <- parse_mungepiece(list(list(NULL, predict_fn), predict_arg1, predict_arg2 = "blah"))
#'
#' # If we wish to run different arguments but the same function during
#' # training versus prediction.
#' piece <- parse_mungepiece(list(train = list(train_fn, train_arg1),
#'                                predict = list(train_fn, predict_arg1)))
#'
#' # If we wish to run different arguments with different functions during
#' # training versus prediction.
#' piece <- parse_mungepiece(list(train = list(train_fn, train_arg1),
#'                                predict = list(predict_fn, predict_arg1)))
#'
#' # The munge function uses the format defined in parse_mungepiece to create
#' # and execute a list of mungepieces on a dataset.
#' munged_data <- munge(raw_data, list(
#'   "Drop useless vars" = list(list(drop_vars, vector_of_variables),
#'                              list(drop_vars, c(vector_variables, "dep_var"))),
#'   "Impute variables"  = list(imputer, imputed_vars),
#'   "Discretize vars"   = list(list(discretize, restore_levels), discretized_vars)
#' ))
#' 
#' # Here, we have requested to munge the raw_data by dropping useless variables,
#' # including the dependent variable dep_var after model training,
#' # imputing a static list of imputed_vars, discretizing a static list
#' # of discretized_vars being careful to use separate logic when merely
#' # using the computed discretization cuts to bin the numeric features into
#' # categorical features. The end result is a munged_data set with an 
#' # attribute "mungepieces" that holds the list of mungepieces used for
#' # munging the data, and can be used to perform the exact same set of
#' # operations on a single row dataset coming through in a real-time production
#' # system.
#' munged_single_row_of_data <- munge(single_row_raw_data, munged_data)
#' # The munge function uses the attached "mungepieces" attribute, a list of
#' # trained mungepieces.
parse_mungepiece <- function(args) {
  stopifnot(is.list(args), length(args) > 0)

  ## The third permissible format requires no unnamed arguments, since it
  ## must be a list consisting of a "train" and "predict" key.
  if (unnamed_count(args) == 0) {
    ## For this case, we delegate the work to `parse_mungepiece_dual`.
    parse_mungepiece_dual(args)
  } else {
    ## Otherwise, the training and prediction arguments are the same.
    parse_mungepiece_single(args)
  }
}

## This is used for the third format.
##
## ```r
## piece <- parse_mungepiece(list(
##   train = list(train_fn, train_arg1),
##   predict = list(train_fn, predict_arg1))
## )
## ```
parse_mungepiece_dual <- function(args) {
  if (!setequal(c("train", "predict"), names(args))) {
    ## This check ensures the list has names exactly equal to
    ## "train" and "predict".
    if (length(args) == 2) {
      error <- paste("Instead, you provided a list with keys",
        paste(sapply(names(args), sQuote), collapse = " and "))
    } else {
      error <- paste0("Instead, you provided a list of length ", length(args))
    }

    ## We use the `m` helper defined in the messages.R file to provide a
    ## descriptive error.
    stop(m("parse_mungepiece_dual_error", error = error))
  }

  ## We use the [`Map`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/funprog.html)
  ## built-in to perform a [`zip`](https://docs.python.org/3/library/functions.html#zip)
  ## operation and translate a pair of `list(train_function, train_args)`
  ## and `list(predict_function, predict_args)` to a pair
  ## `list(train_function, predict_function)` and
  ## `list(train_args, predict_args)`.
  args <- Map(list, parse_mungepiece_dual_chunk(args$train,   type = "train"),
                    parse_mungepiece_dual_chunk(args$predict, type = "predict"))

  ## This is the format we need to use the `mungebit` and `mungepiece`
  ## constructors.
  do.call(mungepiece$new, c(list(do.call(mungebit$new, args[[1L]])), args[[2L]]))
}

## We perform [type dispatch](http://adv-r.had.co.nz/OO-essentials.html#s3) to
## support accepting both a function and a 
## `list(train_or_predict_function, ...)` when we need a non-zero
## number of train or predict arguments.
parse_mungepiece_dual_chunk <- function(args, type) {
  UseMethod("parse_mungepiece_dual_chunk")
}

## This route would have been called for the "train" side if we had done
##
## ```r
## parse_mungepiece(list(train = identity, predict = list(foo, "1")))
## ```
##
## We interpret `train = identity` to mean `train = list(identity)`,
## (i.e. run a mungebit with `identity` as its train function and no
## `train_args` on the mungepiece).
parse_mungepiece_dual_chunk.function <- function(args, type) {
  list(args, list())
}

## In the above example, the `predict` side would be parsed through
## this route.
parse_mungepiece_dual_chunk.list <- function(args, type) {
  ## If there are no unnamed arguments, it violates our convention,
  ## since we are unable to determine the train/predict function.
  if (unnamed_count(args) == 0) {
    stop(m("parse_mungepiece_dual_error_unnamed", type = type))
  }

  fn_index <- unnamed(args)[1L]
  fn       <- to_function(args[[fn_index]], type)
  
  if (!is.function(fn)) {
    stop(m("parse_mungepiece_dual_error_nonfunction", type = type,
           class = class(fn)[1L]))
  }

  ## Otherwise, we extract a list of function and additional arguments,
  ## whether for train or for predict.
  list(fn, args[-fn_index])
}

## If none of the below two formats were passed, we error.
##
## ```r
## parse_mungepiece(list(train = identity, predict = list(foo, "1")))
## ```
parse_mungepiece_dual_chunk.default <- function(args, type) {
  stop(m("parse_mungepiece_dual_error_type", type = type, 
         class = class(args)[1L]))
}

## We will use this parsing strategy if we have an unnamed element, as in:
##
## ```r
## # Using parse_mungepiece_simple
## parse_mungepiece(list(train_fn, ...))
##
## # Using parse_mungepiece_hybrid
## parse_mungepiece(list(list(train_fn, predict_fn), ...))
## ```
parse_mungepiece_single <- function(args) {
  fn_index  <- unnamed(args)[1L]
  ## Extract the first unnamed element and use it as the train/predict function.
  fn <- args[[fn_index]]
  
  if (is.function(fn)) {
    parse_mungepiece_simple(args[-fn_index], fn)
  } else {
    parse_mungepiece_hybrid(args[-fn_index], fn)
  }
}

parse_mungepiece_simple <- function(args, func) {
  ## There is no real work to be done in the simple case
  ## when we call `parse_mungepiece(list(train_fn, ...))`.
  mungepiece$new(mungebit$new(func), args)
}

parse_mungepiece_hybrid <- function(args, funcs) {
  ## If we called `parse_mungepiece(list(list(train_fn, predict_fn), ...))`,
  ## we have to check that the pair in the first element is valid.
  ## It must consist of two functions, one of which (but not both)
  ## may be NULL. The functions could also be substituted with mungebits,
  ## in which the train or predict function (depending on whether the mungebit
  ## is on the left or right hand side) will be extracted. This is not an
  ## encouraged format but is implemented for convenience.
  if (!is.acceptable_hybrid_pair(funcs)) {
    stop(m("parse_mungepiece_hybrid_error"))
  }

  ## We will avoid passing train or predict arguments, respectively,
  ## to the mungepiece constructor if there is no train or predict function,
  ## respectively, to avoid bloating the mungepiece object.
  dual_args <- list(if (!is.null(funcs[[1L]])) args else list(), 
                    if (!is.null(funcs[[2L]])) args else list())

  ## We use the `to_function` helper to extract the train / predict function
  ## in case the user passed a mungebit.
  funcs <- list(to_function(funcs[[1L]], "train"),
                to_function(funcs[[2L]], "predict"))

  do.call(mungepiece$new, c(list(mungebit$new(funcs[[1]], funcs[[2]])), dual_args))
}

to_function <- function(func, type) {
  UseMethod("to_function")
}

to_function.mungebit <- function(func, type) {
  if (type == "train") func$train_function()
  else func$predict_function()
}

to_function.default <- function(func, type) {
  func
}

is.acceptable_hybrid_pair <- function(funcs) {
  ## `funcs` must consist of two functions, one of which (but not both)
  ## may be NULL.
  is.list(funcs) && length(funcs) == 2 &&
  is.acceptable_function(funcs[[1L]]) && 
  is.acceptable_function(funcs[[2L]]) && 
  !(is.null(funcs[[1L]]) && is.null(funcs[[2L]]))
}

