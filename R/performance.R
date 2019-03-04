#' Evaluate performance of prediction algorithm
#' input: pred object, list of measures for performance analysis

performance<-function(pred,measures=list(ppv,tpr,f1))
{
  metrics<-mlr::performance(pred,measures)
  metrics<-as.data.table(as.list(metrics))
  return(metrics)
}