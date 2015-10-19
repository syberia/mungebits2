## *Note*: For better comprehension, this function should be read
## *after* understanding the `mungebit` and `mungepiece`
## classes and the `column_transformation` function defined in this package.
## 
## Recall that in general, transformations of a single data.frame into another
## data.frame fall in three categories:
##
##   1. **Column transformations**. A one-variable function applied to
##      an atomic vector (a column) that yields a new vector of the same length.
##   2. **Row transformations**. A column transformation with a matrix
##      transposition composed to the left and right of it (i.e., operating
##      on rows instead of columns).
##   3. **Whole dataframe transformations**. Any transformation that
##      cannot be expressed as a column or row transformation: for example,
##      a transposition or multiple imputation.
##
## The third class can further be broken down into:
##
##   1. **Multi-column transformations**. Transformations that take a subset
##      of the columns in the dataset and map to another subset of columns
##      (some possibly new).
##
##   2. **Exceptional transformations**. Functions that legitimately require
##      the entire data set as input and cannot be broken down into any
##      of the previous transformations. The nice thing here is that this
##      class includes transposition and similar transformations which
##      are rarely "natural" operations to perform for data wrangling.
##
## The `multi_column_transformation` function is a helper that takes
## a function with a fixed number of arguments, with each argument
## corresponding to a column in the dataset, and produces another set
## of columns. This can be used, for example, to compute feature
## engineering that unifies multiple columns:
##
## ```r
## divider <- multi_column_transformation(function(x, y) {
##   x / y
## })
## new_dataset <- divider(dataset, c("column1", "column2"), "column_ratio")
## # A new variable `column_ratio` is created that is the ratio of 
## # `column1` and `column2`.
## ```
##
## Note the above example can be written more succinctly as
## ``multi_column_transformation(`/`)``, that is, one can pass
## any function to the helper, including primitive R functions.
##
## The function produced, `divider`, accepts a data.frame as its
## first argument and as its second and third argument a vector of column names
## of inputs and outputs, respectively
## (or several other formats; see the `standard_column_format` helper).
##
## If there is more than one output column given, the function should produce
## an ordered or named list giving the output column values.
##
##
## If you think about it, a multi-column transformation with a single input
## and output column that equal each other is actually just a 
## `column_transformation`, so this function is strictly more general.
#' Multi column transformations.
#'
#' A mungebit which takes a fixed group of columns and produces a new
#' group of columns (or a single new column) can be abstracted into a 
#' multi-column transformation. This functions allows one to specify what
#' happens to a fixed list of columns, and the mungebit will be the
#' resulting multi-column transformation applied to an arbitrary combination
#' of columns. An arity-1 multi-column transformation with a single output
#' column equal to its original input column is simply a
#' \code{\link{column_transformation}}.
#' can be abstracted into a column transformation. This function allows one
#' to specify what happens to an individual column, and the mungebit will be
#' the resulting column transformation applied to an arbitrary combination of
#' columns.
#'
#' @param transformation function. The function's first argument will
#'    receive atomic vectors derived from some \code{data.frame}.
#'    Any other arguments will be received as the
#'    \code{list(...)} from calling the function produced by 
#'    \code{multi_column_transformation}.
#' @param nonstandard logical. If \code{TRUE}, nonstandard evaluation support
#'    will be provided for the derived function, so it will be possible
#'    to capture the calling expression for each column. By default \code{FALSE}.
#'    Note this will slow the transformation by 0.1ms on each column.
#' @return a function which takes a data.frame and a vector of column
#'    names (or several other formats, see \code{\link{standard_column_format}})
#'    and applies the \code{transformation}.
#' @seealso \code{\link{column_transformation}}, \code{\link{standard_column_format}}
#' @note The function produced by calling \code{multi_column_transformation} will
#'    not run independently. It must be used a train or predict function for
#'    a \code{\link{mungebit}}.
#' @export
#' @examples
#' divider <- multi_column_transformation(function(x, y) { x / y })
#' # Determines the ratio of Sepal.Length and Sepal.Width in the iris dataset.
#' iris2 <- mungebit$new(divider)$run(iris, c("Sepal.Length", "Sepal.Width"), "Sepal.Ratio") 
#' # Another way to achieve the same thing.
#' iris2 <- mungebit$new(divider)$run(iris, 1:2, "Sepal.Ratio") 
multi_column_transformation <- function(transformation, nonstandard = FALSE) {
}

