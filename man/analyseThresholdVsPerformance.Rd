% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyseThresholdVsPerformance.R
\name{analyseThresholdVsPerformance}
\alias{analyseThresholdVsPerformance}
\title{outputs performance at different threshold for different learning models
Input: benchmark result object(bmr), list of measures (fpr, tpr, ppv,mmce,f1,auc) by default
Output: performance graph
User selects the learning algorithm and the threshold by looking ata the graphs}
\usage{
analyseThresholdVsPerformance(bmr, measures = list(fpr, tpr, ppv, mmce,
  f1, auc))
}
\description{
outputs performance at different threshold for different learning models
Input: benchmark result object(bmr), list of measures (fpr, tpr, ppv,mmce,f1,auc) by default
Output: performance graph
User selects the learning algorithm and the threshold by looking ata the graphs
}
