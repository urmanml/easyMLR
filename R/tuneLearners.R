#'Automatic Hyperparameter tuning using cross-validation of a list of learners
#'Input: lrns(list of learners)
#'Output: lrns(hyperparameter tuned list of learners

tuneLearners<-function(lrns,task)
{
  lrns<-lapply(lrns,FUN =tuningFunction,task)
  return(lrns)
}