#' @include mungepiece-initialize.R mungepiece-run.R
NULL

## Mungebits are intended to record the dichotomy between computations
## that occur at training time, such as computing the means of variables
## during imputation, and prediction time, such as restoring `NA` values
## with the precomputed means at prediction time.
##
## While a mungebit records the general computation that can apply to 
## arbitrary datasets, a *mungepiece* records the training and prediction
## arguments applicable to the mungebit. For example, if we used an imputation
## mungebit that looked as follows
##
## ```r
## imputation_mungebit <- mungebit$new(function(data, columns) {
##   data[columns] <- lapply(columns, function(column) {
##     if (isTRUE(trained)) {
##       input[[column]] <- mean(data[[column]], na.rm = TRUE)
##     }
##     ifelse(is.na(data[[column]]), input[[column]], data[[column]])
##   })
## })
## ```
##
## then we may wish to record the columns to which the imputation 
## applies. In this case, we can use a *mungepiece*.
##
## ```r
## piece <- mungepiece$new(imputation_mungebit, imputed_columns, imputed_columns)
## ```
## 
## To run the mungepiece on our data set we can say `piece$run(data, column_names)`. 
## The advantage of this approach is that after the mungepiece has been trained,
## it will remember the means and can be used on single row data.frames (i.e.,
## those coming in from production) without a change in syntax or calling
## convention. This means that the typical disproportion of spending the
## majority of one's time "munging data" is drastically reduced and no 
## further code has to be written to ensure the transformations run correctly
## in a production setting.
#' Mungepiece.
#'
#' @name mungepiece
#' @docType class
#' @export
mungepiece <- R6::R6Class("mungepiece", 
  public = list(
    .mungebit     = NULL, # mungebit
    .train_args   = NULL, # list
    .predict_args = NULL,

    initialize = mungepiece_initialize,
    run        = mungepiece_run,

    debug      = function() { debug(self$.mungebit) },
    undebug    = function() { undebug(self$.mungebit) },
    trained    = function() { self$.mungebit$trained() },
    mungebit   = function() { self$.mungebit },

    train_args   = function() { env2list(self$.train_args) },
    predict_args = function() { env2list(self$.predict_args) },

    duplicate  = function(...) { duplicate_mungepiece(self, ...) }
  )
)

## A helper used to make a fresh untrained replica of an
## existing mungepiece.
duplicate_mungepiece <- function(piece, ...) {
  mungepiece$new(piece$mungebit()$duplicate(...),
                 piece$train_args(), piece$predict_args())
}

#' Determine whether an object is a mungepiece.
#'
#' @keywords typecheck
#' @param x ANY. An R object to check.
#' @return TRUE or FALSE according as it has class mungepiece
#' @export
is.mungepiece <- function(x) {
  inherits(x, "mungepiece")
}

#' @export
print.mungepiece <- function(x, ...) {
  print_mungepiece(x, ...)
}

