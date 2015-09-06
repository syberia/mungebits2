#' An approach to data preparation that is compatible with production systems.
#'
#' Mungebits2 defines a way of thinking about data preparation that
#' couples the definition of what happens in batch processing versus
#' online prediction so that both can be described by the same codebase.
#'
#' For example, consider the simple example of imputation. While the
#' general concept of imputing a variable works on arbitrary codebases,
#' a \emph{separate} data transformation will have to be defined for
#' each model that uses imputation in a production setting. This is 
#' because the imputed value depends inherently on the dataset.
#' We must remember the mean of the data set encountered during
#' training, and recall this value when performing replacement in
#' a production setting.
#'
#' Mungebits provide a sort of "train track switch" that allows one
#' to write data preparation offline, but ensure it works online 
#' (on a stream of new data, such as one-row data.frames).
#'
#' By reframing data preparation as the process of constructing
#' a "munge procedure", a list of trained mungebits that can
#' reproduce the same mathematical operation on a dataset in
#' a production environment without additional code, the process
#' of productionizing a machine learning model should become
#' significantly simplified.
#'
#' @name mungebits2
#' @import stagerunner crayon
#' @docType package
NULL
