#' Construct a new mungepiece.
#'
#' A mungebit defines an atomic data transformation of an \emph{arbitrary}
#' data set. In order to specify the parameters that may be relevant for
#' a \emph{particular} data set (such as restricting its effect to
#' specific columns, fixing certain parameters such as the imputation
#' method, or providing information that may contain domain knowledge)
#' one uses a mungepiece. A mungepiece defined a \emph{domain-specific}
#' atomic transformation of a data set.
#'
#' A mungepiece is defined by the collection of
#'
#' \enumerate{
#'   \item{A mungebit. }{The mungebit determines the qualitative nature
#'      of the data transformation. The mungebit may represent
#'      a discretization method, principal component analysis,
#'      replacement of outliers or special values, and so on.
#'
#'      If a training set represents automobile data and there are
#'      variables like "weight" or "make," these variables should not be
#'      hardcoded in the mungebit's \code{train} and \code{predict}
#'      functions. The mungebit should only represent that abstract
#'      \emph{mathematical} operation performed on the data set.}
#'   \item{Training arguments. }{While the mungebit represents the code
#'      necessary for performing some \emph{abstract mathematical operation}
#'      on the data set, the training arguments record the metadata
#'      necessary to perform the operation on a \emph{particular}
#'      data set.
#'
#'      For example, if we have an automobile data set and know the
#'      "weight" column has some missing values, we might pass a vector
#'      of column names that includes "weight" to an imputation mungebit
#'      and create an imputation-for-this-automobile-data mungepiece.
#'
#'      If we have a medical data set that includes special patient type
#'      codes and some of the codes were mistyped during data entry or
#'      are synonyms for the same underlying "type," we could pass a list
#'      of character vectors to a "grouper" mungebit that would condense
#'      the categorical feature by grouping like types.
#'
#'      If we know that some set of variables is predictive for modeling a
#'      particular statistical question but are unsure about the remaining
#'      variables, we could use this intuition to pass the list of known
#'      variables as exceptions to a "sure independence screening" mungebit.
#'      The mungebit would run a univariate regression against each variable
#'      not contained in the exceptions list and drop those totally uncorrelated
#'      with the dependent variable. This is a typical technique for high
#'      dimensionality reduction. Knowledge of the exceptions would reduce
#'      the computation time necessary for recording which variables are
#'      nonpredictive, an operation that may be very computationally expensive.
#'
#'      In short, the mungebit records what we are doing to the data set
#'      from an abstract level and does not contain any domain knowledge.
#'      The training arguments, the arguments passed to the mungebit's 
#'      \code{train_function}, record the details that pinpoint the
#'      abstract transformation to a \emph{particular} training set intended for
#'      use with a predictive model.}
#'   \item{Prediction arguments. }{It is important to understand the 
#'      train-predict dichotomy of the mungebit. If we are performing an
#'      imputation, the mungebit will record the means computed from the
#'      variables in the training set for the purpose of replacing \code{NA}
#'      values. The training arguments might be used for specifying the columns
#'      to which the imputation should be restricted.
#'
#'      The prediction arguments, by default the same as the training arguments,
#'      are metadata passed to the mungebit's \code{predict_function}, such as
#'      again the variables the imputation applies to. Sometimes the prediction
#'      arguments may differ slightly from the training arguments, such as when
#'      handling the dependent variable (which will not be present during
#'      prediction) or when the code used for prediction needs some further
#'      parametrization to replicate the behavior of the \code{train_function}
#'      on one-row data sets (i.e., real-time points in a production setting).}
#' } 
#'
#' In short, mungepieces \emph{parametrize} a \emph{single transformation}
#' of a data set \emph{for that particular data set}. While a mungebit is
#' abstract and domain-independent and may represent computations like 
#' imputation, outlier detection, and dimensionality reduction, a mungepiece
#' records the human touch and domain knowledge that is necessary for
#' ensuring the mungebits operate on the appropriate features and optimize
#' space-time tradeoffs (for example, the modeler may know that certain
#' columns do not contain missing values and avoid passing them to the
#' imputation mungebit).
#'
#' Informally speaking, you can think of a mungebit as the \emph{raw mold}
#' for a transformation and a mungepiece as the
#' \emph{cemented product constructed from the mold} that is specific to
#' a particular data set. A mungepiece affixes a mungebit so it works on
#' a specific data set, and domain knowledge may be necessary to construct
#' the mungepiece optimally.
#'
#' @param mungebit mungebit. A mungebit \code{\link{mungebit}} representing
#'    an abstract transformation of a data set, such as type conversion,
#'    imputation, outlier detection, dimensionality reduction,
#'    value replacement, and so on.
#' @param train_args list. Arguments to pass to the mungebit when it is
#'    run for the first time, i.e., on a \emph{training set} that will be
#'    fed to a predictive model and may be quite large. These arguments,
#'    passed directly to the mungebit's \code{train_function}, should 
#'    contain domain-specific metadata that is necessary to apply the
#'    mungebit to this specific data set.
#'
#'    For example, if the modeler knows certain columns do not contain 
#'    missing values, they might pass a character vector of column names
#'    to an imputation mungebit that avoids attempting to impute the
#'    columns guaranteed to be fully present. Doing this heuristically might
#'    require an unnecessary pass over the data, potentially expensive if
#'    the data consists of thousands of features; domain-specific knowledge
#'    might be used to pinpoint the few features that require imputation.
#' @param predict_args list. Arguments to pass to the mungebit when it
#'    is run for the second or subsequent time, i.e., on a \code{prediction set}
#'    that will usually be coming from model validation code or a real-time
#'    production environment. After the mungebit has been trained on the
#'    training set, it should be capable of predicting on
#'    \emph{single row data sets}, i.e., new "points" coming through in
#'    a live production setting.
#'
#'    Usually, the prediction arguments will be the same as the training
#'    arguments for the mungepiece.
#' @examples
#' \dontrun{
#'   doubler <- mungebit$new(column_transformation(function(x) x * 2))
#'   cols    <- c("Sepal.Length", "Petal.Length")
#'   mp      <- mungepiece$new(doubler, list(cols))
#'   iris2   <- mp$run(iris)
#'   stopifnot(identical(iris2[cols], 2 * iris[cols]))
#' }
mungepiece_initialize <- function(mungebit     = NULL,
                                  train_args   = list(),
                                  predict_args = train_args) {

  if (!is.mungebit(mungebit)) {
    stop("To create a new mungepiece, please pass a ",
         sQuote("mungebit"), " as the first argument. I received ",
         "something of class ", sQuote(crayon::red(class(mungebit)[1L])), ".")
  }

  if (!is.list(train_args)) {
    stop("To create a new mungepiece, please pass a list (of training ",
         "arguments) as the second argument. I received something of ",
         "class ", sQuote(crayon::red(class(train_args)[1L])), ".")
  }

  if (!is.list(predict_args)) {
    stop("To create a new mungepiece, please pass a list (of training ",
         "arguments) as the second argument. I received something of ",
         "class ", sQuote(crayon::red(class(train_args)[1L])), ".")
  }

  self$.mungebit     <- mungebit
  self$.train_args   <- make_env(train_args)
  self$.predict_args <- make_env(predict_args)

  lockEnvironment(self$.train_args,   bindings = TRUE)
  lockEnvironment(self$.predict_args, bindings = TRUE)
}

