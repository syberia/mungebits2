initialize <- run <- train <- predict <- function(...) { }

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
#'   col * inputs$scale
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
    .inputs           = NULL, # Environment
    .trained          = NULL, # Logical
    .enforce_train    = NULL, # Logical

    initialize = initialize,
    run        = run,
    train      = train,
    predict    = predict
  )
)

