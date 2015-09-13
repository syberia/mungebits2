## Running a mungepiece respects the same laws as running a mungebit.
## During training, the goal is to record the necessary metadata the
## mungebit needs in order to run during prediction (i.e., on one
## row data sets in a real-time production environment).
##
## The first time `mungepiece$run` is called, the call is delegated
## to the `mungebit` attached to the mungepiece with the appropriate
## training arguments. 
##
## For example, imagine we have a mungebit that discretizes a variable.
##
## ```r
## 
## ```
mungepiece_run <- NULL

