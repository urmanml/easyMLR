


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