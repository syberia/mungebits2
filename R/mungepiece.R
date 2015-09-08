## Mungebits are intended to record the dichotomy between computations
## that occur at training time, such as computing the means of variables
## during imputation, and prediction time, such as restoring `NA` value
## with the precomputed means at prediction time.
##
## While a mungebit records the general computation that can apply to 
## arbitrary datasets, a *mungepiece* records the training and prediction
## arguments applicable to the mungebit. For example, if we used an imputation
## mungebit that looked as follows
##
## ```r
## imputation_mungebit <- mungebit(function(data, columns) {
##   data[columns] <- lapply(columns, function(column) {
##     if (isTRUE(trained)) {
##       input[[column]] <- mean(data[[column]], na.rm = TRUE)
##     }
##     ifelse(is.na(data[[column]]), input[[column]], data[[column]])
##   })
## })
## ```
##
## then we may wish to record the columns to which the imputation 
## applies. In this case, we can use a *mungepiece*.
##
## ```r
## mungepiece <- mungepiece(imputation_mungebit, imputed_columns, imputed_columns)
## ```
## 
## In this case, we can call `mungepiece$run(iris, "Sepal.Width")`.
NULL
