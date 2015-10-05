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
#' standard_column_format('Sepal.Length', iris)  # 'Sepal.Length'
#' standard_column_format(list(is.numeric, c(1,5)), iris)  # 'Sepal.Length'
#' # TODO: (RK) Explain except()
standard_column_format <- function(cols, dataframe) {
  missingcols <- missing(cols)
  if (missingcols) colnames(dataframe)
  else {
    process1 <- function(subcols) {
      if (is(subcols, "except")) {
        unexcepted <- unexcept(subcols)
        if (!is.list(unexcepted)) unexcepted <- list(unexcepted)
        setdiff(colnames(dataframe), process(unexcepted))
      } else if (is.function(subcols)) {
        # Much faster than lapply here.
        colnames(dataframe)[(function() {
          ix <- logical(length(dataframe))
          for (i in seq_along(dataframe)) ix[i] <- subcols(.subset2(dataframe, i))
          ix
        })()]
      }
      else if (is.character(subcols)) force(subcols) 
      else if (is.list(subcols)) process(subcols)
      else colnames(dataframe)[subcols]
    }

    process <- function(xcols) {
      Reduce(intersect, lapply(xcols, process1))
    }

    if (is(cols, "except")) {
      setdiff(colnames(dataframe), process(list(unexcept(cols))))
    } else if (is.list(cols)) {
      process(cols)
    } else {
      process1(cols)
    }
  }
}

#' Ignore during standard column format.
#'
#' @param x ANY. An R object.
#' @export
except <- function(x) {
  class(x) <- c('except', class(x))
  x
}

unexcept <- function(x) {
  class(x) <- setdiff(class(x), 'except')
  x
}

