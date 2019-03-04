#'outputs performance at different threshold for different learning models
#'Input: benchmark result object(bmr), list of measures (fpr, tpr, ppv,mmce,f1,auc) by default
#'Output: performance graph
#'User selects the learning algorithm and the threshold by looking ata the graphs



analyseThresholdVsPerformance<-function(bmr,measures=list(fpr, tpr, ppv,mmce,f1,auc))
{
  df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr, ppv,mmce,f1,auc))
  plotROCCurves(df)
  plot<-plotThreshVsPerf(df)
  return(plot)
}