#' Constructor for mungebit class.
#'
#' Mungebits are atomic data transformations of a data.frame that,
#' loosely speaking, aim to modify "one thing" about a variable or
#' collection of variables. This is pretty loosely defined, but examples
#' include dropping variables, mapping values, discretization, etc.
#'
#' @param train_function function. This specifies the behavior to perform
#'    on the dataset when preparing for model training. A value of NULL
#'    specifies that there should be no training step, i.e., the data
#'    should remain untouched.
#' @param predict_function function. This specifies the behavior to perform
#'    on the dataset when preparing for model prediction. A value of NULL
#'    specifies that there should be no prediction step, i.e., the data
#'    should remain untouched.
#' @param enforce_train logical. Whether or not to flip the trained flag
#'    during runtime. Set this to FALSE if you are experimenting with
#'    or debugging the mungebit.
#' @examples
#' mb <- mungebit$new(column_transformation(function(column, scale = NULL) {
#'   # `trained` is a helper provided by mungebits indicating TRUE or FALSE
#'   # according as the mungebit has been run on a dataset.
#'   if (!trained) {
#'     cat("Column scaled by ", input$scale, "\n")
#'   } else {
#'     # `input` is a helper provided by mungebits. We remember the
#'     # the `scale` so we can re-use it during prediction.
#'     input$scale <- scale
#'   }
#'   column * input$scale
#' }))
#' 
#' # We make a lightweight wrapper to keep track of our data so
#' # the mungebit can perform side effects (i.e., modify the data without an
#' # explicit assignment <- operator).
#' irisp <- list2env(list(data = iris))
#' #mb$run(irisp, 'Sepal.Length', 2)
#'
#' #head(mp$data[[1]] / iris[[1]])
#' # > [1] 2 2 2 2 2 2
#' #mb$run(mp, 'Sepal.Length')
#' # > Column scaled by 2
#' #head(mp$data[[1]] / iris[[1]])
#' # > [1] 4 4 4 4 4 4 
mungebit_initialize <- function(train_function   = base::identity,
                                predict_function = train_function,
                                enforce_train    = TRUE) {
  stopifnot(isTRUE(enforce_train) || identical(enforce_train, FALSE))

  if (!is.acceptable_function(train_function)) {
    stop("To create a new mungebit, please pass a ",
         sQuote("function"), " as the first argument. I received ",
         "something of class ", sQuote(crayon::red(class(train_function)[1L])), ".")
  }

  if (!is.acceptable_function(predict_function)) {
    stop("To create a new mungebit, please pass a ",
         sQuote("function"), " as the second argument. I received ",
         "something of class ", sQuote(crayon::red(class(predict_function)[1L])), ".")
  }

  self$.train_function   <- train_function
  self$.predict_function <- predict_function
  self$.input            <- new.env(parent = emptyenv())
  self$.trained          <- FALSE
  self$.enforce_train    <- enforce_train
}

