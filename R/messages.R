## A dictionary of messages used by the package.
messages <- list(
  parse_mungepiece_dual_error = c(
    "When using a fully named list to construct a mungepiece, ",
    "it must consist solely of ", sQuote("train"), " and ",
    sQuote("predict"), " elements giving a list with the ",
    "respective train or predict function and any additional ",
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

msg <- function(name) {
  stopifnot(name %in% names(messages))

  paste(collapse = "", gsub("[ ]+", " ", messages[[name]]))
}

m <- function(name, ...) {
  do.call(whisker::whisker.render, list(msg(name), list(...)))
}


