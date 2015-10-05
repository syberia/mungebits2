## *Note*: For better comprehension, this function should be read
## *after* understanding the `mungebit` and `mungepiece`
## classes defined in this package.
## 
## In general, transformations of a single data.frame into another
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
## The `column_transformation` function is a helper that tak 
## a function with at least one argument--the atomic vector (column)
## being operated on, with additional arguments acting as further
## parametrization--and turns that function into a function suitable
## for use with a mungebit that will operate on an entire data.frame.
## For example,
##
## ```r
## stripper <- column_transformation(function(x) {
##   gsub("[[:space:]]", "", x)
## })
## new_dataset <- stripper(dataset, c("column1", "column2"))
## ```
##
## The function produced, `stripper`, accepts a data.frame as its
## first argument and as its second argument a vector of column names
## (or several other formats; see the `standard_column_format` helper).
##
## The argument `name` is reserved, and if you create a column transformation
## from a function that includes this argument, its value will be set
## to the name of the column:
##
## ```r
## adjoin_name <- column_transformation(function(x, name) {
##   paste0(x, "_", name)
## })
## new_dataset <- adjoin_name(dataset, c("column1", "column2"))
## # If column1 and column2 are character vectors, they will now
## # have all their values prefixed with `column1_` and `column2_`,
## # respectively.
## ```
#' Pure column transformations.
#'
#' A mungebit which affects multiple columns identically and independently
#' can be abstracted into a column transformation. This function allows one
#' to specify what happens to an individual column, and the mungebit will be
#' the resulting column transformation applied to an arbitrary combination of
#' columns.
#'
#' @param transformation function. The function's first argument will
#'    receive an atomic vector derived from some \code{data.frame}. If the
#'    \code{transformation} has a \code{name} argument, it will receive
#'    the column name. Any other arguments will be received as the
#'    \code{list(...)} from calling the function produced by 
#'    \code{column_transformation}.
#' @param nonstandard logical. If \code{TRUE}, nonstandard evaluation support
#'    will be provided for the derived function, so it will be possible
#'    to capture the calling expression for each column. By default \code{FALSE}.
#'    Note this will slow the transformation by 0.1ms on each column.
#' @return a function which takes a data.frame and a vector of column
#'    names (or several other formats, see \code{\link{standard_column_format}})
#'    and applies the \code{transformation}.
#' @seealso \code{\link{multi_column_transformation}}, \code{\link{standard_column_format}}
#' @note The function produced by calling \code{column_transformation} will
#'    not run independently. It must be used a train or predict function for
#'    a \code{\link{mungebit}}.
#' @export
#' @examples
#' doubler <- column_transformation(function(x) { 2 * x })
#' # doubles the Sepal.Length column in the iris dataset
#' iris2 <- mungebit$new(doubler)$run(iris, c("Sepal.Length")) 
column_transformation <- function(transformation, nonstandard = FALSE) {
  ## We will construct a function *from scratch*. Since R is almost
  ## [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language\))
  ## under the hood, it is possible to construct a function piece-by-piece.
  ##
  ## In general, an R function [consists of three components](http://adv-r.had.co.nz/Functions.html):
  ##
  ##  * **Formals**. The arguments to the function. You can access these
  ##    for any function using the [`formals`](https://stat.ethz.ch/R-manual/R-patched/library/base/html/formals.html)
  ##    helper. This is a named list of expressions, with the values being
  ##    the defaults for each argument.
  ##  * **Body**. The body of the function. In R, a block of code can be 
  ##    represented within R itself as a `language` object. Specifically,
  ##    using [`quote`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/substitute.html)
  ##    can be used to construct the *body* of a function, as in
  ##    `quote({ a <- 1; print(a); return(a) })`. This is a form of
  ##    [reflection](https://en.wikipedia.org/wiki/Reflection_(computer_programming\)).
  ##  * **Environment**. The R environment the function has access to
  ##    when looking for local variables. In other words, its lexical
  ##    environment [as a closure](https://en.wikipedia.org/wiki/Closure_(computer_programming\)).
  ## 
  ## For a `column_transformation`, its derived transformation will be
  ## a new function that takes a `data` argument and a vector of `columns`,
  ## and executes the `transformation` on each column.
  ##
  ## Note we have to inject a few helpers like `%||%` and `list2env_safe`,
  ## which are defined in the mungebits2 package internals, since these
  ## may not be available when a mungebit is serialized and exported out of
  ## the active R session (if mungebits2 is not attached to the search path).
  full_transformation <- function(data, columns = colnames(data), ...) { }
  environment(full_transformation) <- list2env(
    list(transformation = transformation, nonstandard = isTRUE(nonstandard),
         "%||%" = `%||%`, list2env_safe = list2env_safe),
    parent = globalenv()
  )
  body(full_transformation) <- column_transformation_body
  # Add some convenient metadata for overloading `debug` and `print`.
  class(full_transformation) <- c("column_transformation", "transformation", "function")
  full_transformation
}

## As promised, we specify the *body* for the derived transformation
## generated by calling `column_transformation`. Since this will not
## change, we can store it in the package namespace.
column_transformation_body <- quote({
  # Recall that `data` and `columns` are formals.
  ## In this function, optimization matters. Column transformations will
  ## run millions of times over various datasets, so even microsecond
  ## shaved off is valuable. Throughout, note the code may be
  ## slightly improved for readability but at a speed cost. When
  ## developing new packages, one should follow the old adage to
  ## first make it functional, then make it beautiful, then make
  ## it fast. In this case, we prefer speed over beauty!

  if (!isTRUE(trained)) {
    ## The dataset passed in may look different depending on whether
    ## we are running the mungebit in train or predict mode. If 
    ## `columns` are `1:4` and the dataset is shuffled, we will
    ## be referencing the wrong columns after running the mungebit
    ## the second time in predict mode! To avoid this problem, keeping
    ## in mind that R data.frames have unique column names by design,
    ## we store the *character vector of column names* in the mungebit
    ## input so that we know exactly which columns this transformation
    ## should apply to in predict mode.
    ##
    ## If you require operating totally different column names during
    ## training versus prediction, it is by definition not the same mathematical
    ## transformation, and thus a mungebit is likely not the appropriate
    ## tool for your problem.
    input$`_columns` <- intersect(colnames(data), standard_column_format(columns, data))
  }

  if (length(colnames(data)) != length(unique(colnames(data)))) {
    duplicate_names <- utils::head(colnames(data)[duplicated(colnames(data))], 5)
    stop("Cannot run a ", sQuote("column_transformation"), " on data ",
         "with duplicate column names: ",
         paste(vapply(duplicate_names, crayon::red, character(1)), collapse = ", "))
  }

  # An optimization trick to avoid the slow `[.data.frame` operator.
  old_class   <- class(data)
  class(data) <- "list" 

  new_transformation <- transformation
  named      <- is.element("name", names(formals(transformation)))
  arguments  <- c(list(NULL), eval(substitute(alist(...))))
  parent_env <- environment(transformation) %||% baseenv()
  if (nonstandard) {
    data_expr <- substitute(data)
  }

  for (column_name in input$`_columns`) {
    if (isTRUE(trained)) {
      mock_input <- list2env_safe(input[[column_name]])
      lockEnvironment(mock_input, bindings = TRUE)
    } else {
      mock_input <- new.env(parent = emptyenv())
    }

    environment(new_transformation) <- list2env(
      list(input = mock_input, trained = trained),
      parent = parent_env 
    )
    if (isdebugged(transformation)) debug(new_transformation)

    if (named) {
      arguments$name <- column_name
    }

    if (nonstandard) {
      # Support non-standard evaluation at a slight speed cost.
      arguments[[1L]] <- bquote(.(data_expr)[[.(column_name)]])
    } else {
      arguments[[1L]] <- data[[column_name]]
    }

    data[[column_name]] <- do.call(new_transformation, arguments, envir = parent.frame())

    if (!isTRUE(trained)) {
      input[[column_name]] <- as.list(mock_input)
    }
  }

  ## Because we stripped the `data` of its `data.frame` class earlier,
  ## we will have to remove `NULL` values manually (usually `[.data.frame`,
  ## the `data.frame` subsetting operator, handles it for us).
  ## This method of removal preserves attributes, an important fact
  ## to keep in mind once you understand the `munge` function.
  class(data) <- old_class
  data
})

