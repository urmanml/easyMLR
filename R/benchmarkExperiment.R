#'Benchmark different models on the training data set using cross-validation
#'Input: lrns(list of learners), task, list of measures 
#'Output: lrns(hyperparameter tuned list of learners-(fnr,tpr,f1,auc) by default


benchmarkExperiment<-function(lrns,task,measures=list(fnr,tpr,f1,auc))
{
  rdesc  <- makeResampleDesc("CV",iters = 3L,stratify = logical(1))
  bmr = mlr::benchmark(lrns, task, rdesc, measures)
  bmr
  
  return(bmr)
}