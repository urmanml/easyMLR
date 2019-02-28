#' outlier treatment function
outlierTreatment<-function(dataset,target)
{
  #currently not capping any values, additional arguments need to be passed
  dataset<-capLargeValues(dataset,target = target)
  return(dataset)
}