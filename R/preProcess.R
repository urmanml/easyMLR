#' Function for preprocessing data
#' turn character into factors

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
