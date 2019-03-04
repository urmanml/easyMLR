



executionHelper<-function()
{
  dataset <- readline(prompt="Step 1: read data\nSample code: read.csv()\n    ")
  print(colnames(dataset))
  target<- readline(prompt="Step 2: specify target: \n Sample code: target_column\n")
}