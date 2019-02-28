#' Treat Class Imbalance. Default rate is 8
treatClassImbalance<-function(task,rate=8)
{
  task = smote(task, rate = rate, nn = 5)
}