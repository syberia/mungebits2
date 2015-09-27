## A dictionary of messages used by the package.
## We separate these into a separate file to avoid cluttering
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


