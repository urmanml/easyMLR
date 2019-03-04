






featureSelection<-function(task)
{
  #filtered.task = filterFeatures(task, method = "FSelectorRcpp_information.gain", abs = 2)
  filtered.task = filterFeatures(task, method = 'information.gain', perc = 1.0)
  #filtered.task = filterFeatures(task, method = "information.gain", threshold = 0.5)
  return(filtered.task)
}