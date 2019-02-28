

#' Function for missing value imputation
#' missing value imputation based on the class of variable


missingValueImputation<-function(dataset,target)
{
  imp = impute(dataset,target = target, classes = list(integer = imputeMean(),numeric=imputeMean(), factor = imputeMode()),
               dummy.classes = c("integer","numeric"))

  dataset<-imp$data
  return(dataset)
}
