#' normalize features
featureNormalization<-function(dataset,target)
{
dataset<-normalizeFeatures(dataset, target = target, method = "range",
                           #cols = NULL,
                           range = c(0, 1), on.constant = "quiet")
return(dataset)
}