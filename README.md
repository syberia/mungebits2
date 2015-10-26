Mungebits 2 [![Build Status](https://travis-ci.org/robertzk/mungebits2.svg?branch=master)](https://travis-ci.org/robertzk/mungebits2) [![Coverage Status](https://coveralls.io/repos/robertzk/mungebits2/badge.svg?branch=master&service=github)](https://coveralls.io/r/robertzk/mungebits2) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/mungebits2/)
===========

Mungebits2 define a way of thinking about data preparation that couples the definition
of what happens in batch processing versus online prediction so that
both can be described by the same codebase. This means you can use R
to run your munging in a production system on streaming single rows of data
without writing additional code.

This is a re-implementation of [mungebits](https://github.com/robertzk/mungebits)
that removes the need for non-standard evaluation, and provides better integration with
[stageRunner](https://github.com/robertzk/stagerunner).

The package has full test coverage and documentation, so you are
encouraged to peek into the internals.

## Philosophy

The idea behind mungebits grew out of a year-long session 
attempting to productionize R code without translating it into
another programming language.

Almost every package that implements a statistical predictor
requires the user to provide a *wrangled* dataset, that is, one
stripped of outliers, with correctly coerced types, and an array
of other "data preparation" aspects that may affect the final
performance of the model.

Consider, for example, making use of a categorical variable that
has many unique values, some of which occur commonly and others
incredibly rarely. It may improve performance of some classifiers
to take the rare values, say those which occur with a frequency
of less than 5% in the data set, and label them as the value 
"OTHER".

The choice of which variables make it into the "OTHER"
label is determined by the training set, which may differ across
random cross-validation splits and change as an organization 
gathers more data or the distribution shifts, such as due to
a changing consumer base or market conditions.

When one refits a model with the new dataset, it would be ideal if
the data preparation *automatically* reflected the updated values
by picking the set of labels that occur with greater than 5%
frequency and labeling all others as "OTHER".

In code, we may say that

```r
during_training <- function(factor_column) {
  frequencies <- table(factor_column)
  most_common <- names(which(frequencies / length(factor_column) > 0.05))
  factor_column <- factor(
    ifelse(factor_column %in% most_common, factor_column, "OTHER"),
    levels = c(most_common, "OTHER")
  )
  list(new_column = factor_column, most_common = most_common)
}

# Let's create an example variable.
factor_column <- factor(rep(1:20, 1:20))
output <- during_training(factor_column)
factor_column <- output$new_column

# We would hold on to output$most_common and "feed it" to
# munging code that ran in production on single data points.
during_prediction <- function(factor_column, most_common) {
  factor(ifelse(factor_column %in% most_common, factor_column, "OTHER"),
    levels = c(most_common, "OTHER"))
}

# Notice we have re-used our earlier code for constructing the new
# column. We will have to use the above function for munging in
# production and supply it the list `most_common` levels computed
# earlier during training.

single_data_point <- 5
stopifnot(identical(
  during_prediction(5, output$most_common),
  factor("OTHER", levels = c(as.character(11:20), "OTHER"))
))

single_data_point <- 15
stopifnot(identical(
  during_prediction(15, output$most_common),
  factor("15", levels = c(as.character(11:20), "OTHER"))
))

# In a real setting, we would want to operate on full data.frames
# instead of only on atomic vectors.
```

It may seem silly to create a factor variable with a single value
and a surplus of unused levels, but that is only the case if you
have never tried to productionize your data science models! Remember,
even if you trained a simple regression, your factor columns will need
to be converted to 0/1 columns using something like the `model.matrix`
helper function, and this will yell at you if the correct levels are not
there on the factor column.

The point of mungebits is to replace all that hard work--which in the
experience of the author has sometimes spanned data preparation procedures
composed of *hundreds* of steps like the above for collections of
*thousands* of variables--with the much simplified

```r
# During offline training.
replace_uncommon_levels_mungebit$run(dataset)
```

The mungebit has now been "trained" and remembers the `common_levels`
defined earlier. In a production system, we will be able to run the
exact same code on a single row of data, as long as we serialize
the mungebit object and recall it during production. This gives us
a streaming machine learning engine that includes hard data
wrangling work--in R.

```r
# During real-time prediction.
replace_uncommon_levels_mungebit$run(dataset)
```

After understanding mungebits, data science will stop being data
janitor work and you will get back to the math.

## Examples

For more examples of practical mungebits, see any of the functions
exported in the [syberiaMungebits](https://github.com/robertzk/syberiaMungebits)
repository. Examples include

 * [Parsing dates](https://github.com/robertzk/syberiaMungebits/blob/master/R/date_parser.r)
 * [Discretization](https://github.com/robertzk/syberiaMungebits/blob/master/R/discretizer.r)
 * [Dropping sparse variables](https://github.com/robertzk/syberiaMungebits/blob/master/R/drop_percent_missing.r)
 * [Imputation](https://github.com/robertzk/syberiaMungebits/blob/master/R/imputer.r)
 * [Deriving new variables](https://github.com/robertzk/syberiaMungebits/blob/master/R/new_variable.R)
 * [Ordering by a key](https://github.com/robertzk/syberiaMungebits/blob/master/R/orderer.r)
 * [Stripping outliers](https://github.com/robertzk/syberiaMungebits/blob/master/R/remove_outliers.r)
 * [Renaming variables](https://github.com/robertzk/syberiaMungebits/blob/master/R/renamer.r)
 * [Selecting a subset of rows](https://github.com/robertzk/syberiaMungebits/blob/master/R/select_rows.r)
 * [Sure independence screening](https://github.com/robertzk/syberiaMungebits/blob/master/R/sure_independence_screen.r)
 * [Replacing values](https://github.com/robertzk/syberiaMungebits/blob/master/R/value_replacer.r)
 * Handling special codes in character data
 * And so on...

## Installation

This package is not yet available from CRAN (as of October 26, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/mungebits2")
```

### License

This project is licensed under the MIT License:

Copyright (c) 2015-2016 Robert Krzyzanowski

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Authors

This package was created by Robert Krzyzanowski, technoguyrob@gmail.com.
It is based on the original package [mungebits](https://github.com/robertzk/mungebits)
by the same author. 

