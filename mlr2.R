

preProcess<-function(dataset)
{
  dataset<-as.data.table(dataset)
  str(dataset)
  summary(dataset)

  dataset<-dataset[,-c("V1")]
  #dataset<-sapply(dataset,as.factor)
  dataset$loan_status<-as.factor(dataset$loan_status)
  dataset$grade<-as.factor(dataset$grade)
  dataset$home_ownership<-as.factor(dataset$home_ownership)


  dataset <- as.data.frame(dataset)
  #https://mlr.mlr-org.com/articles/tutorial/impute.html
  return(dataset)
}
missingValueImputation<-function(dataset,target)
{
  imp = impute(dataset,target = target, classes = list(integer = imputeMean(),numeric=imputeMean(), factor = imputeMode()),
               dummy.classes = c("integer","numeric"))

  dataset<-imp$data
  return(dataset)
}
outlierTreatment<-function(dataset,target)
{
  #currently not capping any values, additional arguments need to be passed
  dataset<-capLargeValues(dataset,target = target)
  return(dataset)
}
featureNormalization<-function(dataset,target)
{
dataset<-normalizeFeatures(dataset, target = target, method = "range",
                           #cols = NULL,
                           range = c(0, 1), on.constant = "quiet")
return(dataset)
}
treatClassImbalance<-function(task,rate=8)
{
  task = smote(task, rate = rate, nn = 5)
}
bestLearnerIndex<-function(bmr)
{
  max=0
  maxi=0
  for  (i in 1:length(bmr))
  {
    if(max<bmr$results$dataset[[i]][['aggr']])

    {
      max<-bmr$results$dataset[[1]][['aggr']]
      maxi<-i
    }

  }
  return(maxi)
}
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
paramSet<-function(lrn)
{#https://mlr.mlr-org.com/articles/tutorial/tune.html
  getParamSet(lrn)

  if(lrn$id=="classif.rpart"){
    print("training rpart")
    gs <- makeParamSet(
      makeIntegerParam("minsplit",lower = 10, upper = 10),
      makeIntegerParam("minbucket", lower = 5, upper = 10),
      makeNumericParam("cp", lower = 0.01, upper = 0.02)
    )
  }



  else if(lrn$id=="classif.svm"){
    print("training svm")
    gs <- makeParamSet(
      makeNumericParam("gamma",lower = -1, upper = 1, trafo = function(x) 10^x),
      makeDiscreteParam("kernel",values = c("polynomial")),
      makeNumericParam("cost", lower = -1, upper = 1, trafo = function(x) 10^x),
      makeIntegerParam("degree",lower = 2,upper=2)
    )
  }


  else if (lrn$id=="classif.nnet"){
    print("training svm")
    gs <- makeParamSet(
      makeIntegerParam("size",lower = 3, upper = 3),
      makeIntegerParam("maxit", lower = 10, upper = 100)
    )
  }
  else if (lrn$id=="classif.logreg"){
    getParamSet("classif.logreg")
    print("training logreg")
    gs <- makeParamSet(
      makeLogicalParam("model",default = TRUE)
    )
  }
  return(gs)
}
