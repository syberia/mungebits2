## Usually, we wish to perform more than one cleaning operation on a dataset
## before it is ready to be fed to a machine learning classifier.
##
## The `munge` function defined in this file allows for a quick way to
## apply a list of mungepieces to a dataframe. 
##
## ```r
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
## Instead of building the mungepieces and bits by hand by calling the
## `mungepiece$new` and `mungebit$new` constructors (which is another 
## alternative), we use this convenient format to construct and apply
## the mungepieces on-the-fly.
##
## The end result is the `munged_data`,
## which is the final cleaned and ready-to-model data, together with
## an [attribute](https://stat.ethz.ch/R-manual/R-devel/library/base/html/attributes.html)
## "mungepieces" which stores the list of trained mungepieces.
## In other words, the munged data *remembers* how it was obtained
## from the raw data. The list of trained mungepieces, informally called a
## **munge procedure**, can be used to replicate the munging in a real-time
## streaming production system without remember the full training set:
##
## ```r
## munged_single_row <- munge(single_row, attr(munged_data, "mungepieces"))
##
## # A syntactic shortcut enabled by the munge helper. It knows to look for
## # the mungepieces attribute.
## munged_single_row <- munge(single_row, munged_data)
## ```
##
## We can feed single rows of data (i.e., streaming records coming through
## in a production system) to the trained munge procedure and it will
## correctly replicate the same munging it performed during model training.
#' Apply a list of mungepieces to data.
#'
#' The \code{munge} function allows a convenient format for applying a 
#' sequence of \code{\link{mungepiece}} objects to a dataset. 
#' 
#' The \code{munge} helper accepts a raw, pre-munged (pre-cleaned)
#' dataset and a list of lists. Each sublist represents the code
#' and hyperparameters necessary to clean the dataset. For example,
#' the first row could consist of an imputation function and a list
#' of variables to apply the imputation to. It is important to
#' understand what a \code{\link{mungebit}} and \code{\link{mungepiece}}
#' does before using the \code{munge} helper, as it constructs these
#' objects on-the-fly for its operation.
#'
#' The end result of calling \code{munge} is a fully cleaned data set
#' (i.e., one to whom all the mungepieces have been applied and trained)
#' adjoined with a \code{"mungepieces"} attribute: the list of trained
#' mungepieces.
#'
#' For each sublist in the list of pre-mungepieces passed to \code{munge},
#' the following format is available. See the examples for a more hands-on
#' example.
#'   
#' \enumerate{
#'   \item{\code{list(train_fn, ...)}}{ -- If the first element of \code{args} is
#'     a function followed by other arguments, the constructed mungepiece
#'     will use the \code{train_fn} as both the \emph{train and predict}
#'     function for the mungebit, and \code{list(...)} (that is, the remaining 
#'     elements in the list) will be used as both the train and predict
#'     arguments in the mungepiece. In other words, using this format
#'     specifies you would like \emph{exactly the same behavior in
#'     training as in prediction}. This is appropriate for mungebits
#'     that operate in place and do not need information obtained
#'     from the training set, such as simple value replacement or
#'     column removal.
#'   }
#'   \item{\code{list(list(train_fn, predict_fn), ...)}}{
#'     -- If \code{args} consists of a two-element pair in its first
#'     element, it must be a pair of either \code{NULL}s or functions,
#'     with not both elements \code{NULL}. If the \code{train_fn}
#'     or \code{predict_fn}, resp., is \code{NULL}, this will signify to have
#'     \emph{no effect} during training or prediction, resp.
#'      
#'     The remaining arguments, that is \code{list(...)}, will be used
#'     as both the training and prediction arguments.
#' 
#'     This structure is ideal if the behavior during training and prediction
#'     has an identical parametrization but very different implementation,
#'     such as imputation, so you can pass two different functions.
#'
#'     It is also useful if you wish to have no effect during prediction,
#'     such as removing faulty rows during training, or no effect during
#'     training, such as making a few transformations that are only
#'     necessary on raw production data rather than the training data.
#'   }
#'   \item{\code{list(train = list(train_fn, ...), predict = list(predict_fn, ...))}}{
#'     If \code{args} consists of a list consisting of exactly two named
#'     elements with names "train" and "predict", then the first format will be
#'     used for the respective fields. In other words, a mungepiece will
#'     be constructed consisting of a mungebit with \code{train_fn} as the
#'     training function, \code{predict_fn} as the predict fuction, and
#'     the mungepiece train arguments will be the train list of additional
#'     arguments \code{list(...)}, and similarly the predict arguments will be
#'     the predict list of additional arguments \code{list(...)}.
#'
#'     Note \code{train_fn} and \code{predict_fn} must \emph{both} be functions
#'     and not \code{NULL}, since then we could simply use the second format
#'     described above.
#'
#'     This format is ideal when the parametrization differs during training and
#'     prediction. In this case, \code{train_fn} usually should be the same
#'     as \code{predict_fn}, but the additional arguments in each list can
#'     be used to identify the parametrized discrepancies. For example, to
#'     sanitize a dataset one may wish to drop unnecessary variables. During
#'     training, this excludes the dependent variable, but during prediction
#'     we may wish to drop the dependent as well.
#'
#'     This format can also be used to perform totally different behavior on
#'     the dataset during training and prediction (different functions and
#'     parameters), but mungebits should by definition achieve the same
#'     operation during training and prediction, so this use case is rare
#'     and should be handled carefully.
#'   }
#' }
#'
#' @export
#' @param data data.frame. Raw, uncleaned data.
#' @param mungelist list. A list of lists which will be translated to a
#'    list of mungepieces. It is also possible to pass a list of mungepieces,
#'    but often the special syntax is more convenient. See the examples section.
#' @param stagerunner logical or list. Either \code{TRUE} or \code{FALSE}, by default
#'    the latter. If \code{TRUE}, a \code{\link[stagerunner]{stagerunner}}
#'    object will be returned whose context will contain a key \code{data}
#'    after being ran, namely the munged data set (with a "mungepieces"
#'    attribute).
#'
#'    One can also provide a list with a \code{remember} parameter,
#'    which will be used to construct a stagerunner with the same value
#'    for its \code{remember} parameter.
#' @return A cleaned \code{data.frame}, the result of applying each
#'    \code{\link{mungepiece}} constructed from the \code{mungelist}.
#' @seealso \code{\link{mungebit}}, \code{\link{mungepiece}},
#'    \code{\link{parse_mungepiece}}
#' @examples
#' # First, we show off the various formats that the parse_mungepiece
#' # helper accepts. For this exercise, we can use dummy train and
#' # predict functions and arguments.
#' train_fn   <- predict_fn   <- function(x, ...) { x }
#' train_arg1 <- predict_arg1 <- dual_arg1 <- TRUE # Can be any parameter value.
#' 
#' # The typical way to construct mungepieces would be using the constructor.
#' piece <- mungepiece$new(
#'   mungebit$new(train_fn, predict_fn),
#'   list(train_arg1), list(predict_arg1)
#' )
#'
#' # This is tedious and can be simplified with the munge syntax, which
#' # allows one to specify a nested list that defines all the mungebits
#' # and enclosing mungepieces at once.
#' 
#' raw_data <- iris
#' munged_data <- munge(raw_data, list(
#'   # If the train function with train args is the same as the predict function
#'   # with predict args, we use this syntax. The first element should be
#'   # the funtion we use for both training and prediction. The remaining
#'   # arguments will be used as both the `train_args` and `predict_args`
#'   # for the resulting mungepiece.
#'   "Same train and predict" = list(train_fn, train_arg1, train_arg2 = "blah"),
#'  
#'   # If the train and predict arguments to the mungepiece match, but we
#'   # wish to use a different train versus predict function for the mungebit.
#'   "Different functions, same args" =
#'     list(list(train_fn, predict_fn), dual_arg1, dual_arg2 = "blah"),
#'   
#'   # If we wish to only run this mungepiece during training.
#'   "Only run in train" = list(list(train_fn, NULL), train_arg1, train_arg2 = "blah"),
#'   
#'   # If we wish to only run this mungepiece during prediction.
#'   "Only run in predict" = list(list(NULL, predict_fn), predict_arg1, predict_arg2 = "blah"),
#'  
#'   # If we wish to run different arguments but the same function during
#'   # training versus prediction.
#'   "Totally different train and predict args, but same functions" = 
#'      list(train = list(train_fn, train_arg1),
#'           predict = list(train_fn, predict_arg1)),
#'  
#'   # If we wish to run different arguments with different functions during
#'   # training versus prediction.
#'   "Totally different train and predict function and args" = 
#'     list(train = list(train_fn, train_arg1),
#'                       predict = list(predict_fn, predict_arg1))
#' )) # End the call to munge()
#'
#' # This is an abstract example that was purely meant to illustrate syntax
#' # The munged_data variable will have the transformed data set along
#' # with a "mungepieces" attribute recording a list of trained mungepieces
#' # derived from the above syntax.
#'  
#' # A slightly more real-life example.
#' \dontrun{
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
#' }
#' # The munge function uses the attached "mungepieces" attribute, a list of
#' # trained mungepieces.
munge <- function(data, mungelist, stagerunner = FALSE) {
  stopifnot(is.data.frame(data))

  if (length(mungelist) == 0L) {
    return(data)
  }

  if (!is.list(mungelist)) {
    stop(m("munge_type_error", class = class(mungelist)[1L]))
  }

  if (is.data.frame(mungelist)) {
    if (!is.element("mungepieces", names(attributes(mungelist)))) {
      stop(m("munge_lack_of_mungepieces_attribute"))
    }
    Recall(data, attr(mungelist, "mungepieces"), stagerunner)
  } else if (is(mungelist, "tundraContainer")) {
    # An optional interaction effect with the tundra package.
    Recall(data, mungelist$munge_procedure, stagerunner)
  } else {
    munge_(data, mungelist, stagerunner)
  }
}

# Assume proper arguments.
munge_ <- function(data, mungelist, stagerunner) {
  runners <- vapply(mungelist, is, logical(1), "stageRunner")
  # TODO: (RK) Intercept errors and inject with name for helpfulness!
  mungelist[!runners] <- lapply(mungelist[!runners], parse_mungepiece)
  stages <- mungepiece_stages(mungelist)

  if (is.environment(data)) {
    context <- data
  } else {
    context <- list2env(list(data = data), parent = emptyenv())
  }

  remember <- is.list(stagerunner) && isTRUE(stagerunner$remember)
  runner <- stagerunner::stageRunner$new(context, stages, remember = remember)

  if (identical(stagerunner, FALSE)) {
    runner$run()
    context$data
  } else {
    runner
  }
}

mungepiece_stages <- function(mungelist, contiguous = FALSE) {
  if (!isTRUE(contiguous)) {
    singles <- which(vapply(mungelist, Negate(is), logical(1), "stageRunner"))
    groups  <- cumsum(diff(c(singles[1L] - 1, singles)) != 1)
    split(mungelist[singles], groups) <- lapply(
      split(mungelist[singles], groups), mungepiece_stages, contiguous = TRUE
    )
    mungelist
  } else {
    mungepiece_stages_contiguous(mungelist)
  }
}

mungepiece_stages_contiguous <- function(mungelist) {
  shared_context <- list2env(parent = globalenv(),
    list(size = length(mungelist), mungepieces = mungelist,
         newpieces = list())
  )

  lapply(seq_along(mungelist), mungepiece_stage, shared_context)
}

mungepiece_stage <- function(mungepiece_index, context) {
  stage <- function(env) {
    # Make a fresh copy to avoid shared stage problems.
    piece <- mungepieces[[mungepiece_index]]$duplicate(private = TRUE)
    piece$run(env)
    newpieces[[mungepiece_index]] <<- piece

    if (mungepiece_index == size) {
      attr(env$data, "mungepieces") <-
        append(attr(env$data, "mungepieces"), newpieces)
    }
  }
  environment(stage) <- list2env(parent = context,
    list(mungepiece_index = mungepiece_index)
  )
  stage
}
