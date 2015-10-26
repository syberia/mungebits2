## Let's say we called `munge` with
##
## ```r
## munge(data, list(column_transformation(function(x) 2 * x), 1:3))
## ```
##
## This will double the *first three columns*. Alternatively, we could say
##
## ```r
## munge(data, list(column_transformation(function(x) 2 * x),
##                  list(is.numeric, 1:3)))
## ```
##
## where the innermost `list` serves as *conjunction* and says
## "of the first three columns, double those which *are numeric*."
## The `standard_column_format` helper figures out this logic given
## the data set and the selection, e.g. `1:3` or `list(is.numeric, 1:3)`.
#' Converts a logical / numeric / character vector or a function
#' into a character vector of column names for a dataframe.
#'
#' If a function is provided, it will be applied to each column of
#' the dataframe and must return a logical; those with resulting value TRUE
#' will be returned as a character vector.
#'
#' @param cols a vector or function. If logical / numeric / character vector,
#'    it will attempt to find those columns matching these values. If \code{cols}
#'    is a function, it will apply this function to each column of the dataframe
#'    and return the names of columns for which it was \code{TRUE}. Additionally,
#'    \code{cols} can be a list of any combination of the above, which will 
#'    require all the conditions to be met.
#' @param dataframe a reference dataframe. Necessary for computing the
#'    column names if a numeric or logical vector is specified for \code{cols}.
#' @export
#' @examples
#' standard_column_format(c(1,5), iris)  # c('Sepal.Length', 'Species')
#' standard_column_format(c(TRUE,FALSE,FALSE,FALSE,TRUE), iris)  # c('Sepal.Length', 'Species')
## This function is rather messy, but we cannot split up its body as it
## will be injected into functions generated using `column_transformation`,
## which should be portable even when the mungebits package is not installed
## (for example, if the user wishes to send their list of mungepieces to
## a friend, or install it in production).
#' standard_column_format('Sepal.Length', iris)  # 'Sepal.Length'
#' standard_column_format(list(is.numeric, c(1,5)), iris)  # 'Sepal.Length'
#' # TODO: (RK) Explain except()
standard_column_format <- function(cols, dataframe) {
  ## If no columns are provided, we assume we are running on the entire data set.
  if (missing(cols)) colnames(dataframe) 
  else {
    process1 <- function(subcols) {
      ## Let's say we called `munge` with
      ##
      ## ```r
      ## munge(data, list(column_transformation(as.numeric), is.character))
      ## ```
      ##
      ## to convert our remaining `character` columns to `numeric`s. However,
      ## say we are performing a multi-class classification and expect the
      ## dependent variable, `dep_var`, to be character but *not* numeric.
      ## We can then include it as an exception:
      ##
      ## ```r
      ## munge(data, list(column_transformation(as.numeric),
      ##                  list(except("dep_var"), is.character)))
      ## ```
      ##
      ## If we had simply written `except("dep_var")`, it would mean
      ## "all variables except `dep_var`", or if we had written
      ## `except(is.character)`, it would mean "all variables except
      ## the `character` variables." 
      if (is(subcols, "except")) {
        unexcepted <- unexcept(subcols)
        if (!is.list(unexcepted)) unexcepted <- list(unexcepted)
        setdiff(colnames(dataframe), process(unexcepted))
      } else if (is.function(subcols)) {
        # Much faster than lapply here.
        colnames(dataframe)[local({
          ix <- logical(length(dataframe))
          if (is.element("name", names(formals(subcols)))) {
            for (i in seq_along(dataframe)) {
              ix[i] <- subcols(.subset2(dataframe, i), name = .subset2(colnames(dataframe), i))
            }
          } else {
            ## The `[` and `[[` operator internally call out to 
            ## `.subset2`, which references the actual C function
            ## and is thus faster (avoiding unnecessary checks).
            for (i in seq_along(dataframe)) ix[i] <- subcols(.subset2(dataframe, i))
          }
          ix
        ## If you scroll up a little, the section for `is.function(subcols)`
        ## applies to the notation
        ##
        ## ```r
        ## munge(data, list(column_transformation(as.numeric), is.character))
        ## ```
        ##
        ## where we take the `is.character` function and apply it to each
        ## column. It must always return `TRUE` or `FALSE` and will only
        ## apply the `column_transformation` to columns satisfying the condition.
        })]
      }
      else if (is.character(subcols)) force(subcols) 
      else if (is.list(subcols)) process(subcols)
      else colnames(dataframe)[subcols]
    }

    process <- function(xcols) {
      Reduce(intersect, lapply(xcols, process1))
    }

    ## Lots of recursion tricks here! Even if I tried to explain what is
    ## going on, I'd likely fail, so just take this function home and
    ## study it. Usually, we'd break it up into many smaller pieces, 
    ## but as mentioned before that would be inconvenient here since
    ## we must include it in full in `column_transformation`s to ensure
    ## they are portable.
    if (is(cols, "except")) {
      setdiff(colnames(dataframe), process(list(unexcept(cols))))
    } else if (is.list(cols)) {
      process(cols)
    } else {
      process1(cols)
    }
  }
}

## We use the `"except"` S3 class to tag any inputs to
## `standard_column_format` whose meaning should be *negated* 
## (i.e., do *not* apply to these columns).
#' Ignore during standard column format.
#'
#' @param x ANY. An R object.
#' @export
except <- function(x) {
  class(x) <- c("except", class(x))
  x
}

unexcept <- function(x) {
  class(x) <- setdiff(class(x), "except")
  x
}

