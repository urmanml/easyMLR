#'Find the best model in the benchmark result
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