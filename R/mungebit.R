#' @include mungebit-initialize.R mungebit-run.R mungebit-train_predict.R
NULL

## The idea behind mungebits grew out of a year-long session 
## attempting to productionize R code without translating it into
## another programming language.
##
## Almost every package that implements a statistical predictor
## requires the user to provide a *wrangled* dataset, that is, one
## stripped of outliers, with correctly coerced types, and an array
## of other "data preparation" aspects that may affect the final
## performance of the model.
##
## Consider, for example, making use of a categorical variable that
## has many unique values, some of which occur commonly and others
## incredibly rarely. It may improve performance of some classifiers
## to take the rare values, say those which occur with a frequency
## of less than 5% in the data set, and label them as the value 
## "OTHER".
##
## The choice of which variables make it into the "OTHER"
## label is determined by the training set, which may differ across
## random cross-validation splits and change as an organization 
## gathers more data or the distribution shifts, such as due to
## a changing consumer base or market conditions.
##
## When one refits a model with the new dataset, it would be ideal if
## the data preparation *automatically* reflected the updated values
## by picking the set of labels that occur with greater than 5%
## frequency and labeling all others as "OTHER".
##
## In code, we may say that
##
## ```r
## during_training <- function(factor_column) {
##   frequencies <- table(factor_column)
##   most_common <- names(which(frequencies / length(factor_column) > 0.05))
##   factor_column <- factor(
##     ifelse(factor_column %in% most_common, factor_column, "OTHER"),
##     levels = c(most_common, "OTHER")
##   )
##   list(new_column = factor_column, most_common = most_common)
## }
##
## # Let's create an example variable.
## factor_column <- factor(rep(1:20, 1:20))
## output <- during_training(factor_column)
## factor_column <- output$new_column
## 
## # We would hold on to output$most_common and "feed it" to
## # munging code that ran in production on single data points.
## during_prediction <- function(factor_column, most_common) {
##   factor(ifelse(factor_column %in% most_common, factor_column, "OTHER"),
##     levels = c(most_common, "OTHER"))
## }
## 
## # Notice we have re-used our earlier code for constructing the new
## # column. We will have to use the above function for munging in
## # production and supply it the list `most_common` levels computed
## # earlier during training.
##
## single_data_point <- 5
## stopifnot(identical(
##   during_prediction(5, output$most_common),
##   factor("OTHER", levels = c(as.character(11:20), "OTHER"))
## ))
## 
## single_data_point <- 15
## stopifnot(identical(
##   during_prediction(15, output$most_common),
##   factor("15", levels = c(as.character(11:20), "OTHER"))
## ))
##
## # In a real setting, we would want to operate on full data.frames
## # instead of only on atomic vectors.
## ```
## 
## It may seem silly to create a factor variable with a single value
## and a surplus of unused levels, but that is only the case if you
## have never tried to productionize your data science models! Remember,
## even if you trained a simple regression, your factor columns will need
## to be converted to 0/1 columns using something like the `model.matrix`
## helper function, and this will yell at you if the correct levels are not
## there on the factor column.
## 
## The point of mungebits is to replace all that hard work--which in the
## experience of the author has sometimes spanned data preparation procedures
## composed of *hundreds* of steps like the above for collections of
## *thousands* of variables--with the much simplified
##
## ```r
## # During offline training.
## replace_uncommon_levels_mungebit$run(dataset)
## ```
##
## The mungebit has now been "trained" and remembers the `common_levels`
## defined earlier. In a production system, we will be able to run the
## exact same code on a single row of data, as long as we serialize
## the mungebit object and recall it during production. This gives us
## a streaming machine learning engine that includes hard data
## wrangling work--in R.
##
## ```r
## # During real-time prediction.
## replace_uncommon_levels_mungebit$run(dataset)
## ```
##
## After understanding mungebits, data science will stop being data
## janitor work and you will get back to the math.
#' Mungebits are atomic data transformations that are amenable to productionization.
#'
#' The majority of data projects are overcome by the burden of excessive
#' data wrangling. Part of the problem lies in the fact that when new
#' data is introduced that was drawn from the same source as the original,
#' such as a training set for a statistical model, \emph{different} code
#' needs to be written to achieve the same transformations. Mungebits solve
#' this problem by forcing the user to determine how to correctly munge
#' on out-of-sample data (such as live streaming data in the form of one-row
#' data.frames) at "munge-time", when the reason for the wrangling is still
#' apparent. A frequent source of data errors is treating this process as an
#' afterthought.
#'
#' Consider the following problem. Imagine we wish to discretize a variable,
#' say determined algorithmically with cuts [0, 0.5), [0.5, 1.5), [1.5, 3).
#' When we apply the same transformation on a new data set, we cannot run
#' the same discretization code, since it may produce new cutoffs, and hence
#' invalidate the results if, for example, we had trained a model on the
#' prior cutoffs. To ensure the exact same mathematical transformation
#' is performed on new data--whether a new test set derived from recent
#' data or a one-row data.frame representing a single record streaming
#' through a production system--we must run \emph{different code} on
#' the "original" set versus the new set.
#'
#' Mathematically speaking, a transformation of a data set can be represented
#' by a single mathematical function that is implemented differently during
#' "training" versus "prediction." Here, "training" refers to the first
#' time the transformation is performed, and "prediction" refers to 
#' subsequent times, such as on newly obtained data or a one-row data.frame
#' representing a single new record in a production system.
#'
#' Therefore, the \emph{correct} approach to data preparation, if you
#' wish to re-use it in the future on new data sets or in a live production
#' environment, is to treat it as a collection of tuples
#' \code{(train_function, predict_function, input)}, where
#' \code{train_function} represents the original code, \code{input} represents
#' an arbitrary R object such as a list, used for storing "metadata"
#' necessary to re-create the original transformation, and the
#' \code{predict_function} takes this \code{input} metadata and produces
#' the identical transformation on an out-of-sample data set.
#'
#' For example, if we wish to impute a data set, \code{train_function}
#' might compute the mean, store it in \code{input$mean}, replace
#' the \code{NA} values with the mean, and return the dataset. Meanwhile,
#' the \code{predict_function} simply replaces the \code{NA} values
#' with the cached \code{input$mean}.
#'
#' Usually, these steps would be in disjoint code bases: the modeler
#' would perform the ad-hoc munging while playing with the dataset,
#' and a software engineer would take the computed \code{input$mean}
#' and hard code it into a "data pipeline". It would be infeasible
#' to recompute the mean on-the-fly since \emph{it depends on the
#' original data set}, which may be prohibitively large. However,
#' while it may require a lot of space and time to compute the
#' original \code{input}, as they are parameterized potentially by
#' a very large data set, usually the \code{input} itself is small
#' and the resulting \code{predict_function} is inexpensive. 
#'
#' The fundamental problem of data preparation, and the reason why
#' \href{http://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html}{data scientists spend over 90\% of their time on data preparation},
#' is a lack of respect for this dichotomy. Using mungebits makes
#' this duality blatantly apparent in all circumstances and will hopefully
#' reduce the amount of time wasted on cumbersome wrangling.
#'
#' @docType class
#' @format NULL
#' @name mungebit
#' @export
#' @examples
#' \dontrun{
#' mb <- mungebit(column_transformation(function(col, scale = NULL) {
#'   if (!isTRUE(trained)) { # trained is an injected keyword
#'    cat("Column scaled by ", input$scale, "\n")
#'   } else {
#'    input$scale <- scale
#'   }
#'  
#'   col * input$scale
#' }))
#' 
#' iris2 <- mb$run(iris, "Sepal.Length", 2)
#' # iris2 now contains a copy of iris with Sepal.Length doubled.
#' iris3 <- mb$run(iris2, "Sepal.Length")
#' # > Column scaled by 2
#' head(iris3[[1]] / iris[[1]])
#' # > [1] 4 4 4 4 4 4 
#' }
mungebit <- R6::R6Class("mungebit",
  public = list(
    .train_function   = NULL, # Function or NULL
    .predict_function = NULL, # Function or NULL
    .input            = NULL, # Environment
    .trained          = FALSE, # Logical
    .enforce_train    = TRUE, # Logical

    initialize = mungebit_initialize,
    run        = mungebit_run,
    train      = mungebit_train,
    predict    = mungebit_predict,

    debug      = function() { debug(self) },
    undebug    = function() { undebug(self) },
    trained    = function() { self$.trained },
    input      = function() { as.list(self$.input) }
  )
)

