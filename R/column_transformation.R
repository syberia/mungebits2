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
## The `column_transformation` function is a helper that takes
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
##
## TODO: (RK) List many more examples here and explain column transformation
## standard column format usage.
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
  was_debugged <- isdebugged(transformation)
  environment(transformation) <- list2env(list(
    input = NULL, trained = NULL, has_no_null = NULL
  ), parent = environment(transformation) %||% baseenv())

  ## We create a copy of the `standard_column_format` helper
  ## in this package so it can accompany the `column_transformation`
  ## to R sessions even where this package is not present.
  standard_column_format_dup <- standard_column_format
  environment(standard_column_format_dup) <- globalenv()

  environment(full_transformation) <- list2env(
    list(transformation = transformation, nonstandard = isTRUE(nonstandard),
         "%||%" = `%||%`, list2env_safe = list2env_safe,
         named = is.element("name", names(formals(transformation))),
         env = environment(transformation), was_debugged = was_debugged,
         standard_column_format = standard_column_format_dup),
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
  ## 
  ## If we are supporting non-standard evaluation, we precompute
  ## the expression used, or we will lose it upon first reference of `data`.
  if (nonstandard) {
    data_expr <- substitute(data)
  }

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
    input$columns <- intersect(colnames(data), standard_column_format(columns, data))
  }

  indices <- match(input$columns, names(data))

  # An optimization trick to avoid the slow `[.data.frame` operator.
  old_class   <- class(data)
  ## Try to run ``print(`[.data.frame`)`` from your R console. Notice how
  ## much code is run to perform data.frame subsetting! The same is
  ## true for ``print(`[[<-.data.frame`)``, data.frame element assignment.
  ## Since we use this operation below, we want to skip over the typical
  ## checks for the sake of performance and use straight-up list subsetting
  ## (which will use underlying C code).
  class(data) <- "list" 

  env$trained <- trained
  
  ## If we wish to pass along the expression the transformation was called
  ## with so we can use `substitute` correctly, the only effective way
  ## to perform this capture is to use [`alist`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.html)
  ## and retain the `parent.frame()` during `do.call` below.
  if (nonstandard) {
    arguments  <- c(list(NULL), eval(substitute(alist(...))))
    eval_frame <- parent.frame()
  }

  ## If the mungebit has not been trained yet (recall that `trained` is
  ## injected during the `mungebit$train` and `mungebit$predict` functions),
  ## we create a vector of environments, one for each column the 
  ## `transformation` is run on, so that each respective run has access
  ## to an `input` environment to store computations that will be required
  ## during predict (e.g., storing the mean of the column during imputation).
  if (!isTRUE(trained)) {
    input$sub_inputs <- structure(replicate(
      length(input$columns), new.env(parent = emptyenv()), simplify = FALSE
    ), .Names = input$columns)
  }

  ## Dataframe subset assignment (`[<-.data.frame`) does not behave in the
  ## same manner as list assignment (`[<-`). Since we stripped the data.frame
  ## of its class earlier, the next line will perform *list assignment*. 
  ## This has advantageous speedups, but in particular if we drop some of
  ## the columns by including `NULL` in the output of the transformation,
  ## this will corrupt the data.frame with actual `NULL` values 
  ## instead of dropping columns. We work around this with performance 
  ## considerations by recording whether any of the values in the inner loop
  ## are `NULL`.
  env$has_no_null <- TRUE

  data[indices] <- lapply(seq_along(indices), function(j, ...) {
    ## Since `indices` match the column names to iterate over on
    ## the nose, `sub_inputs[[j]]` will be the correct environment to
    ## use for the jth column. Here, `.subset2` is a trick to speed
    ## things up a tiny bit by calling the C function that does the
    ## actual subsetting.
    env$input <- .subset2(.subset2(input, "sub_inputs"), j)

    ## Assigning a function's environment clears its internal debug 
    ## flag, so if the function was previously being debugged we
    ## retain this property.
    if (was_debugged) {
      debug(transformation)
    }

    ## And the non-standard evaluation trick! Imagine a user had called
    ## a column transformation with the code below.
    ##
    ## ```r
    ## ct <- column_transformation(nonstandard = TRUE, function(x) { y <- substitute(x) })
    ## some_data <- data.frame(first = 1:2, second = c("a", "b"))
    ## mungebit$new(ct)$run(some_data)
    ## ```
    ##
    ## Then `substitute(x)` would be precisely the expression 
    ## `some_data[["first"]]` during the first call and `some_data[["second"]]`
    ## during the second call (in other words, it is equivalent to
    ## `y <- quote(some_data[["first"]])` in the first call, etc.).
    # Support non-standard evaluation at a slight speed cost.
    if (nonstandard) {
      if (named) {
        ## Recall that if the `transformation` has a formal argument called
        ## "name", we must pass along the column name.
        arguments$name <- .subset2(names(data), .subset2(indices, j))
      }

      ## We replace the first argument with the column to apply the transformation
      ## to.
      arguments[[1L]] <- bquote(.(data_expr)[[.(
        if (named) arguments$name else .subset2(names(data), .subset2(indices, j))
      )]])
      result <- .Internal(do.call(transformation, arguments, eval_frame))
    } else {
      ## If NSE should not be carried over we do not bother with the
      ## magic and simply send the function the value.
      if (named) {
        result <- transformation(.subset2(data, .subset2(indices, j)), ...,
                       name = .subset2(names(data), .subset2(indices, j)))
      } else {
        result <- transformation(.subset2(data, .subset2(indices, j)), ...)
      }
    }
    
    ## Using a `has_no_null` flag is slightly faster than `has_null`,
    ## since we can save on a call to `!` in the condition below.
    if (env$has_no_null && is.null(result)) {
      env$has_no_null <- FALSE
    }

    result
  }, ...)

  ## After training, we lock the `input` environments so that the
  ## user cannot modify them during predict.
  if (!isTRUE(trained)) {
    lapply(input$sub_inputs, lockEnvironment, bindings = TRUE)
  }

  ## Finally, if some of the columns *were* dropped, explicitly 
  ## remove them from the dataframe using `[[<-` list assignment.
  ## This ensures that we do not drop any attributes and is faster
  ## than subsetting to non-`NULL` columns.
  if (!env$has_no_null) {
    for (i in which(vapply(data, is.null, logical(1)))) {
      data[[i]] <- NULL
    }
  }

  ## Finally, we reset the class to `data.frame` after stripping it
  ## for a speed optimization. If you study the code of ``(`[.data.frame`)``,
  ## you will see this is exactly the same trick the R base library uses
  ## to delegate to the list subsetting after the data.frame-specific
  ## checks have been completed.
  class(data) <- old_class
  data
})

#' @export
print.column_transformation <- function(x, ...) {
  # `print_transformation` parameters include `indent = 0L, full = FALSE`.
  print_transformation(x, ..., byline = "Column transformation")
}

#' @method all.equal transformation
#' @export
all.equal.transformation <- function(target, current, ...) {
  identical(parent.env(environment(target))$transformation,
            parent.env(environment(current))$transformation)
}

