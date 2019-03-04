#' Used for prediction using the trained model
#' input: threshold, trained model object(mod), test_task (containing test data) 
#' output: pred object

prediction<-function(mod,test_task,threshold=.5)
{
  pred <- predict(mod,test_task)
  pred = setThreshold(pred, threshold = threshold)
  return(pred)
  
}