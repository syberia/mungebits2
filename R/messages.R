## A dictionary of messages used by the package.
## We separate these into its own file to avoid cluttering
## the R code with a multitude of strings.
messages <- list(
  parse_mungepiece_dual_error = c(
    "When using a fully named list to construct a mungepiece, ",
    "it must consist solely of ", sQuote("train"), " and ",
    sQuote("predict"), " elements giving a list with the ",
    "respective train or predict function and any additional ",
    ## Note the use of `{{{error}}}` in conjuction with
    ## [whisker](https://github.com/edwindj/whisker) below.
    "train or predict arguments. ", crayon::red("{{{error}}}."),
    " For example,\n\n",
    crayon::green(paste("list(train = list(discretize, cuts = 5),",
                        "predict = list(restore_levels))")),
    "\n\nwill specify to use the ", sQuote("discretize"),
    " function in training with the argument ",
    sQuote("cuts"), " equal to 5, while the ",
    sQuote("restore_levels"), " function will be used without ",
    "arguments during prediction.\n"
  ),

  parse_mungepiece_dual_error_type = c(
    "When using the explicit train/predict syntax to construct a mungepiece ",
    "you must pass a list on both sides:\n\n",
    crayon::green(paste("list(train = list(discretize, cuts = 5),",
                        "predict = list(restore_levels))")),
    "\n\nInstead, I got a ", crayon::red("{{{class}}}"), " on the ",
    "{{{type}}} side."
  ),

  parse_mungepiece_dual_error_unnamed = c(
    "When using the explicit train/predict syntax to construct a mungepiece ",
    "you must have at least one unnamed element on the {{{type}}} side ",
    "which will be used for the {{{type}}} function."
  ),

  parse_mungepiece_dual_error_nonfunction = c(
    "When using the explicit train/predict syntax to construct a mungepiece, ",
    "the first unnamed element on the {{{type}}} side must be a function. ",
    "Instead, I got a ", crayon::red("{{{class}}}"), "."
  ),

  parse_mungepiece_hybrid_error = c(
    "When using a non-function as the first argument when constructing a ",
    "mungepiece, the only accepted format is a pair of two functions, ",
    "with either one but not both NULL.\n\n",
    crayon::green(paste("list(list(discretize, restore_levels), variables)")),
    "\n\nThe first function will be used for training and the second for ",
    "prediction. Please double check your syntax."
  )
)

## Cleanse the message a little after fetching it from the `messages` list.
msg <- function(name) {
  stopifnot(name %in% names(messages))

  ## The `gsub` will squish multiple spaces into a single space,
  ## while the `paste(collapse = "", ...)` usage will ensure we
  ## can take vectors of characters in the above `messages` list.
  paste(collapse = "", gsub("[ ]+", " ", messages[[name]]))
}

## We use the [whisker](https://github.com/edwindj/whisker) templating
## engine to inject any additional values into the message string.
## For example,
## 
## ```r
## m("parse_mungepiece_dual_error", error = "Bloop")
## ```
##
## would return the appropriate error with the string "Bloop" injected
## in the appropriate place.
m <- function(name, ...) {
  ## Note the use of [`do.call`](http://www.inside-r.org/r-doc/base/do.call),
  ## a very handy R metaprogramming tool when we do not know exactly which 
  ## arguments we will pass.
  do.call(whisker::whisker.render, list(msg(name), list(...)))
}


