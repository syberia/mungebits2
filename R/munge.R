## Usually, we wish to perform more than one cleaning operation on a dataset
## before it is ready to be fed to a machine learning classifier.
##
## The `munge` function defined in this file allows for a quick way to
## apply a list of mungepieces to a dataframe. 
##
## ```r
## munged_data <- munge(raw_data, list(
##   "Drop useless vars" = list(list(drop_vars, vector_of_variables),
##                              list(drop_vars, c(vector_variables, "dep_var"))),
##   "Impute variables"  = list(imputer, imputed_vars),
##   "Discretize vars"   = list(list(discretize, restore_levels), discretized_vars)
## ))
## ```
## 
## Translated in English, we are saying:
##
##   1. Drop a static list of useless variables from the data set.
##      When the model is trained, drop the dependent variable as well
##      since we will no longer need it.
##
##   2. Impute the variables in the static list of `imputed_vars`.
##      When the model is trained, the `imputer` will have some logic
##      to restore the means obtained during training of the mungepiece
##      (assuming we are using mean imputation).
##
##   3. Discretize the static list of variables in the `discretized_vars`
##      character vector. After model training, when new data points come in,
##      the original training set is no longer available. The `discretize`
##      method stored the necessary cuts for each variable in the mungebit's
##      `input`, which the `restore_levels` function uses to bin the
##      numerical features in the list of `discretized_vars` into factor
##      (categorical) variables.
##
## Instead of building the mungepieces and bits by hand by calling the
## `mungepiece$new` and `mungebit$new` constructors (which is another 
## alternative), we use this convenient format to construct and apply
## the mungepieces on-the-fly.
##
## The end result is the `munged_data`,
## which is the final cleaned and ready-to-model data, together with
## an [attribute](https://stat.ethz.ch/R-manual/R-devel/library/base/html/attributes.html)
## "mungepieces" which stores the list of trained mungepieces.
## In other words, the munged data *remembers* how it was obtained
## from the raw data. The list of trained mungepieces, informally called a
## **munge procedure**, can be used to replicate the munging in a real-time
## streaming production system without remember the full training set:
##
## ```r
## munged_single_row <- munge(single_row, attr(munged_data, "mungepieces"))
##
## # A syntactic shortcut enabled by the munge helper. It knows to look for
## # the mungepieces attribute.
## munged_single_row <- munge(single_row, munged_data)
## ```
##
## We can feed single rows of data (i.e., streaming records coming through
## in a production system) to the trained munge procedure and it will
## correctly replicate the same munging it performed during model training.
#' Munge.
#'
#' @param ... Stuff.
#' @export
munge <- function(...) { }
