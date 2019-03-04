
#'editable template function to create new features 

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