
chooseDataset<-function()
{
  dataset<-file.choose()
  return(dataset)
}
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
featureEngineering<-function(dataset)
{
  ####
  dataset<-as.data.table(dataset)
  #Requirement specific feature creation functions called here
  #feature1<-feature1Function(dataset)
  #dataset<-dataset[':='(feature1=feature1,feature2=feature2)]
  ####
  dataset<-as.data.frame(dataset)
  return(dataset)
}
featureNormalization<-function(dataset,target)
{
dataset<-normalizeFeatures(dataset, target = target, method = "range",
                           #cols = NULL, 
                           range = c(0, 1), on.constant = "quiet")
return(dataset)
}
createTask<-function(dataset,target,type)
{
  if (type=="classif")
  {
  task = makeClassifTask(data = dataset, target = target,positive = '1')
  return(task)
    }
  if (type=="regr")
  {
  task = makeRegrTask(data = dataset, target = target)
  return(task)
  }
  if (type=="cluster")
  {
  task = makeClusterTask(data = dataset)
  return(task)
  }
  return("task must be of 'Classif', 'regr' or 'cluster' type")
}
createLearnerList<-function(type=task$type)  
{
  if (type=="classif")
  {
  lrns = list(
    makeLearner("classif.rpart",predict.type = 'prob')
    #,makeLearner("classif.svm",predict.type = 'prob',predict.threshold = 0.7639981)
    ,makeLearner("classif.gbm",predict.type = 'prob')
    #,makeLearner("classif.xgboost",predict.type = 'prob')
  )
  }
  else if (type=="regr")
  {
    lrns = list(
      
      #,makeLearner("classif.svm",predict.type = 'prob',predict.threshold = 0.7639981)
      ,makeLearner("regr.nnet")
      ,makeLearner("regr.gbm")
    )
  }
  else if (type=="cluster")
  {
    lrns = list(
      makeLearner("cluster.kmeans")
      #,makeLearner("classif.svm",predict.type = 'prob',predict.threshold = 0.7639981)
         )
  }
  return(lrns)
}



featureSelection<-function(task)
{
  #filtered.task = filterFeatures(task, method = "FSelectorRcpp_information.gain", abs = 2)
  filtered.task = filterFeatures(task, method = 'information.gain', perc = 1.0)
  #filtered.task = filterFeatures(task, method = "information.gain", threshold = 0.5)
  return(filtered.task)
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
  param_set<-getParamSet(lrn)
  #2) Make param set
  #option a)uncomment this for manually setting the parameters in the paramSet function
  
  gs <- paramSet(lrn)
  #class(gs)
  
  ##option b: using mlrhyperopt
  #gs<- mlrHyperopt::generateParConfig(lrn)$par.set
 
  ##option c: using CaretParamset
  #gs<-getCaretParamSet(lrn$short.name,task = task)$par.set
  
  
  gs
  #3) Specify to do a grid search/vs a random search
  #gscontrol <- makeTuneControlGrid(resolution = 1)
  gscontrol<-makeTuneControlRandom(maxit = 2,tune.threshold = logical(1))
  #4) hypertune the parameters
  stune <- tuneParams(learner = lrn, resampling = rdesc, task = task, par.set = gs, control = gscontrol, measures = tpr)
  
  #5) using hyperparameters for modeling
  lrn <- setHyperPars(lrn, par.vals = stune$x)
  return(lrn)
}
tuneLearners<-function(lrns,task)
{
  lrns<-lapply(lrns,FUN =tuningFunction,task)
  return(lrns)
}
paramSet<-function(lrn)
{#https://mlr.mlr-org.com/articles/tutorial/tune.html
#https://github.com/jakob-r/mlrHyperopt/blob/master/R/getDefaultParSetValues.R
  getParamSet(lrn)
  

  if (lrn$short.name=="boosting"){
    gs <- makeParamSet(
      makeDiscreteParam(id = "coeflearn", default = "Breiman", values = c("Breiman", "Freund", "Zhu")),
      makeNumericParam(id = "mfinal", default = log2(100L/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeIntegerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L)
    )
  }
  
  else if (lrn$short.name=="C50"){
    gs <- makeParamSet(
      makeIntegerParam(id = "trials", lower = 1L, default = 1L, upper = 100L),
      makeLogicalParam(id = "winnow", default = FALSE)
    )
  }
  
  else if (lrn$short.name=="RRF"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, default = expression(floor(p/3)), upper = expression(p)),
      makeNumericParam(id = "coefReg", default = 0.8, lower = 0, upper = 1),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="ada"){
    gs <- makeParamSet(
      makeNumericParam(id = "iter", default = log2(50/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeIntegerParam(id = "maxdepth", default = 10L, lower = 1L, upper = 10L), # mlr recommends 30 as default
      makeNumericParam(id = "nu", default = 0.1, lower = 0.001, upper = 0.5)
    )
  }
  
  else if (lrn$short.name=="blackboost"){
    gs <- makeParamSet(
      makeIntegerParam(id = "maxdepth", default = 2L, lower = 1L, upper = 10L),
      makeNumericParam(id = "mstop", default = log2(100/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10))
    )
  }
  
  else if (lrn$id=="classif.extraTrees"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam(id = "numRandomCuts", default = 1L, lower = 1L, upper = 25L),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.extraTrees"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam(id = "numRandomCuts", default = 1L, lower = 1L, upper = 25L),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="ksvm"){
    gs <- makeParamSet(
      makeNumericParam(id = "C",  upper = 10, lower = -5, trafo = function(x) 2^x, default = log2(1)),
      makeNumericParam(id = "sigma",  upper = 15, lower = -15, trafo = function(x) 2^x, default = expression(kernlab::sigest(as.matrix(getTaskData(task, target.extra = TRUE)[["data"]])), scaled = TRUE)),
      keys = "task"
    )
  }
  
  else if (lrn$short.name=="glmboost"){
    gs <- makeParamSet(
      makeNumericParam(id = "mstop", default = log2(100/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeNumericParam("nu", lower = 0, upper = 1, default = 0.1)
    )
  }
  
  else if (lrn$short.name=="gbm"){
    gs <- makeParamSet(
      makeNumericParam(id = "n.trees", lower = log2(10/10), upper = log2(1000/10), trafo = function(x) round(2^x * 10), default = log2(500/10)),
      makeIntegerParam(id = "interaction.depth", default = 1L, lower = 1L, upper = 10L),
      makeNumericParam(id = "shrinkage", default = 0.001, lower = 0.001, upper = 0.6),
      makeIntegerParam(id = "n.minobsinnode", default = 10L, lower = 5L, upper = 25L)
    )
  }
  
  else if (lrn$short.name=="rpart"){
    gs <- makeParamSet(
      makeNumericParam(id = "cp", default = log2(0.01), lower = -10, upper = 0, trafo = function(x) 2^x),
      makeIntegerParam(id = "maxdepth", default = 30L, lower = 3L, upper = 30L),
      makeIntegerParam(id = "minbucket", default = 7L, lower = 5L, upper = 50L),
      makeIntegerParam(id = "minsplit", default = 20L, lower = 5L, upper = 50L)
    )
  }
  
  else if (lrn$short.name=="nnet"){
    print("training nnet")
    gs <- makeParamSet(
      makeIntegerParam(id = "size", default = 3L, lower = 1L, upper = 20L),
      makeNumericParam(id = "decay", default = 10^(-5), lower = -5, upper = 1, trafo = function(x) 10^x)
    )
  }
  
  else if (lrn$short.name=="glmnet"){
    gs <- makeParamSet(
      makeNumericParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericParam(id = "lambda", default = log2(1), lower = -10, upper = 3, trafo = function(x) 2^x)
    )
  }
  
  else if (lrn$short.name=="xgboost"){
    gs <- makeParamSet(
      makeNumericParam(id = "nrounds", lower = log2(10/10), upper = log2(4000/10), trafo = function(x) round(2^x * 10), default = log2(10/10)),
      makeIntegerParam(id = "max_depth", default = 6L, lower = 1L, upper = 10L),
      makeNumericParam(id = "eta", default = 0.3, lower = 0.001, upper = 0.6),
      makeNumericParam(id = "gamma", default = 0, lower = 0, upper = 10),
      makeNumericParam(id = "colsample_bytree", default = 0.5, lower = 0.3, upper = 0.7),
      makeNumericParam(id = "min_child_weight", default = 1, lower = 0, upper = 20),
      makeNumericParam(id = "subsample", default = 1, lower = 0.25, upper = 1)
    )
  }
  
  else if (lrn$id=="classif.randomForest"){
    gs <- makeParamSet(
      makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(floor(sqrt(p)))),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.randomForest"){
    gs <- makeParamSet(
      makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      keys = "p"
    )
  }
  
  else if (lrn$id=="classif.ranger"){
    gs <- makeParamSet(
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(floor(sqrt(p)))),
      makeIntegerParam("min.node.size", lower = 1, upper = 10, default = 1),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.ranger"){
    
    gs <- makeParamSet(
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam("min.node.size", lower = 1, upper = 10, default = 5),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="svm"){
    gs <- makeParamSet(
      makeNumericParam(id = "cost",  upper = 15, lower = -15, trafo = function(x) 2^x, default = log2(1)),
      makeNumericParam(id = "gamma",  upper = 15, lower = -15, trafo = function(x) 2^x, default = expression(log2(1/p))),
      keys = "p"
    )
  }

  return(gs)
}
benchmarkExperiment<-function(lrns,task,measures=list(fnr,tpr,f1,auc))
{
  rdesc  <- makeResampleDesc("CV",iters = 3L,stratify = logical(1))
  bmr = mlr::benchmark(lrns, task, rdesc, measures)
  bmr
  
  return(bmr)
}
analyseThresholdVsPerformance<-function(bmr,measures=list(fpr, tpr, ppv,mmce,f1,auc))
{
  df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr, ppv,mmce,f1,auc))
  plotROCCurves(df)
  plot<-plotThreshVsPerf(df)
  return(plot)
}
selectLearner<-function(lrns=NULL,name=NULL)
{
  for (i in 1:length(lrns))
  {
    if (lrns[[i]]$short.name==name)
    {
      lrn<-lrns[[i]]
      return(lrn)
    }
  }  
}
train<-function(lrn,task)
{
    mod <- mlr::train(lrn, task)
    return(mod)
}
prediction<-function(mod,test_task,threshold=.5)
{
  pred <- predict(mod,test_task)
  pred = setThreshold(pred, threshold = threshold)
  return(pred)
  
}
performance<-function(pred,measures=list(ppv,tpr,f1))
{
  metrics<-mlr::performance(pred,measures)
  metrics<-as.data.table(as.list(metrics))
  return(metrics)
}
tutorial<-function()
{
  message("Flow")
}
execute<-function(dataset,target)
{
  missingValueImputation
  outlierTreatment
  featureEngineering
  featureNormalization
  createTask
  createLearnerList
  featureSelection
  tuningFunction
  benchmark
  analyseThresholdVsPerformance
  selectLearner
  train
  predict
  performance
}
executionHelper<-function()
{
  dataset <- readline(prompt="Step 1: read data\nSample code: read.csv()\n    ")
  print(colnames(dataset))
  target<- readline(prompt="Step 2: specify target: \n Sample code: target_column\n")
}

