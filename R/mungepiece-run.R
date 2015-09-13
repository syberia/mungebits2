## Running a mungepiece respects the same laws as running a mungebit.
## During training, the goal is to record the necessary metadata the
## mungebit needs in order to run during prediction (i.e., on one
## row data sets in a real-time production environment).
##
## The first time `mungepiece$run` is called, the call is delegated
## to the `mungebit` attached to the mungepiece with the appropriate
## training arguments. 
##
## For example, imagine we have a mungebit that discretizes a variable.
##
## ```r
## discretizer_train <- function(data, columns, breaks = 5) {
##   # Recall that the first argument to a mungebit's train function
##   # is *always* the data set. The additional arguments, in this 
##   # case the column names to discretize, will be the list of
##   # training arguments on the mungepiece.
##   stopifnot(is.character(columns), all(columns %in% colnames(data)))
##
##   # We record the columns that were discretized.
##   input$`_columns` <- columns
##
##   for (column in columns) {
##     # Record the values to discretize at, i.e., the bounds of each interval.
##     quantiles <- quantile(data[[column]], breaks = breaks)
##     # `cuts` will be the discretized variable using R's `base::cut`.
##     cuts <- cut(data[[column]], breaks = quantiles)
##     # We need to remember the cut points and levels to discretize during 
##     # prediction correctly.
##     input[[column]] <- list(cuts = quantiles, levels = levels(cuts)) 
##     # We assume there are no missing values.
##     data[[column]]  <- cuts
##   }
## 
##   data
## }
## 
## # This function will be pretty slow in R. You can rewrite it in Rcpp.
## # It also suffers from a few bugs on the boundaries due to open/closed
## # interval issues, but a full implementation defeats the illustration.
## discretizer_predict <- function(data, columns, ...) {
##   # We leave the last argument as ... in case the user left the train 
##   # arguments the same as the predict arguments so that we may absorb
##   # the `breaks` argument without error.
##   if (missing(columns)) columns <- input$`_columns`
##
##   # We only allow columns that were discretized during training and are
##   # present in the dataset. A more strict approach would throw an error.
##   columns <- intersect(intersect(columns, input$`_columns`), colnames(data))
##   # Some helper functions.
##   coalesce <- function(x, y) { if (length(x) == 0) y[1L] else x[1L] }
##   min2     <- function(x) { if (length(x) == 0) NULL else min(x) }
##
##   for (column in columns) {
##     cuts <- vapply(data[[column]], function(value) {
##       # Convince yourself that `ix` will be the index of the correct
##       # label. For example, if value is `2.5` and `levels` are [0, 1],
##       # (1, 2], (2, 3], (3, 4], then `ix` will be 3.
##       ix <- max(1, coalesce(
##         min2(which(c(-Inf, input[[column]]$cuts[-1L]) >= value)),
##         length(input[[column]]$levels) + 1
##        ) - 1)
##       input[[column]]$levels[ix]
##     }, character(1))
##     data[[column]] <- factor(cuts, levels = input[[column]]$levels)
##   }
## 
##   data   
## }
##
## bit <- mungebit$new(discretizer_train, discretizer_predict)
## ```
##
## Note that the code to implement discretization during training and 
## prediction is quite different! We can turn this mungebit into a 
## mungepiece that operates on the `iris` dataset.
## 
## ```r
## piece <- mungepiece$new(bit, list(c("Sepal.Width", "Sepal.Length")))
## iris2 <- mungepiece$run(iris) # Train the mungepiece.
## head(iris2$Sepal.Length)
## # [1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (5.1,5.8]
## # Levels: (4.3,5.1] (5.1,5.8] (5.8,6.4] (6.4,7.9]
## iris3 <- piece$run(iris[1:6, ]) # It has been trained and run live.
## print(iris3$Sepal.Length)
## # [1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (4.3,5.1] (5.1,5.8]
## # Levels: (4.3,5.1] (5.1,5.8] (5.8,6.4] (6.4,7.9] 
## stopifnot(identical(head(iris2$Sepal.Length), iris3$Sepal.Length))
## ```
##
## The mungepiece also works correctly on outliers.
## ```r
## irisc <- iris; irisc[1:2, 1] <- c(0, 10)
## print(piece$run(irisc[1:2, ])$Sepal.Length)
## # [1] (4.3,5.1] (6.4,7.9]
## # Levels: (4.3,5.1] (5.1,5.8] (5.8,6.4] (6.4,7.9]
## ```
## 
## It is important to handle such cases if new points in a live production
## setting have values that are outside the observed range of the training
## set.
#' Run a mungepiece and prepare it for a live production setting.
#'
#' Running a mungepiece achieves the same effect as running the mungebit
#' attached to the mungepiece: the first time it is run, we \emph{train}
#' the mungebit so it remembers metadata it will need to replicate the
#' operation in a live production setting on a single row of data. The
#' second and subsequent times we run the mungepiece, it will execute
#' the predict function of the underlying mungebit.
#'
#' @inheritParams mungebit_run
#' @return If the \code{data} parameter is an environment, the transformed
#'    environment (i.e., the transformed data in the environment) after 
#'    application of the underlying mungebit. If \code{data} is a data.frame,
#'    the transformed data.frame is returned.
mungepiece_run <- function(data, ..., `_envir` = parent.frame()) {
  if (self$.mungebit$trained()) {
    calling_environment <- self$.predict_args
    reference_function  <- self$.mungebit$predict_function()
  } else {
    calling_environment <- self$.train_args
    reference_function  <- self$.mungebit$train_function()
  }

  args <- eval(substitute(alist(...)))
  args <- two_way_argument_merge(strip_arguments(reference_function, 1),
                                 calling_environment, args)

  parent.env(calling_environment) <- `_envir`
  on.exit(parent.env(calling_environment) <- emptyenv(), add = TRUE)

  args <- c(list(data = substitute(data)), args)

  do.call(self$.mungebit$run, args, envir = calling_environment)
}

strip_arguments <- function(fun, n) {
  formals(fun) <- formals(fun)[setdiff(seq_along(formals(fun)), seq_len(n))]
  fun
}

two_way_argument_merge <- function(reference_function, calling_environment, args) {
  call      <- as.call(c(alist(run), args))
  base_args <- as.list(match.call(reference_function, call)[-1L])

  default_args <- env2listcall(calling_environment)
  names(default_args) <- attr(calling_environment, "initial_names")
  call         <- as.call(c(alist(run), default_args))
  default_args <- as.list(match.call(reference_function, call)[-1L])

  if (unnamed_count(default_args) > 0 && unnamed_count(base_args) > 0) {
    default_args[unnamed(default_args)] <- NULL
  }

  list_merge(default_args, base_args)
}

