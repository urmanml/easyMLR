#'User chooses the learner to be used for training by looking at the output of analyseThresholdVsPerformance() function 

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