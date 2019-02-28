#' hyperparameter tuning function
tuningFunction<-function(lrn,task)
{
  #set 3 fold cross validation
  rdesc <- makeResampleDesc("CV",iters = 3L)
  #1) Search for hyperparameters
  #param_set<-getParamSet("classif.rpart")
  #2) Make param set
  #uncomment this for manually setting the parameters in the paramSet function
  #gs <- paramSet(lrns[[2]])
  #class(gs)
  gs<- mlrHyperopt::generateParConfig(lrn)
  class(gs)
  gs<-gs$par.set
  class(gs)
  #gc<-  generateParConfig("classif.svm")
  #gc$par.set
  #3) Specify to do a grid search/vs a random search
  #gscontrol <- makeTuneControlGrid(resolution = 1)
  gscontrol<-makeTuneControlRandom(maxit = 2,tune.threshold = logical(1))
  #4) hypertune the parameters
  stune <- tuneParams(learner = lrn, resampling = rdesc, task = task, par.set = gs, control = gscontrol, measures = tpr)

  #5) using hyperparameters for modeling
  lrn <- setHyperPars(lrn, par.vals = stune$x)
  return(lrn)
}