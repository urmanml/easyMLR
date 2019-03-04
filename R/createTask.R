



createTask<-function(dataset,target,type)
{
  if (type=="classif")
  {
  task = makeClassifTask(data = dataset, target = target,positive = '1')
  return(task)
    }
  if (type=="regr")
  {
  task = makeRegrTask(data = dataset, target = target)
  return(task)
  }
  if (type=="cluster")
  {
  task = makeClusterTask(data = dataset)
  return(task)
  }
  return("task must be of 'Classif', 'regr' or 'cluster' type")
}