#' Paramset Function
paramSet<-function(lrn)
{#https://mlr.mlr-org.com/articles/tutorial/tune.html
  getParamSet(lrn)

  if(lrn$id=="classif.rpart"){
    print("training rpart")
    gs <- makeParamSet(
      makeIntegerParam("minsplit",lower = 10, upper = 10),
      makeIntegerParam("minbucket", lower = 5, upper = 10),
      makeNumericParam("cp", lower = 0.01, upper = 0.02)
    )
  }

  # else if(lrn$id=="classif.svm"){
  #   gs <- makeParamSet(
  #     makeNumericParam("gamma",lower = 0, upper = 1),
  #     #makeDiscreteParam("kernel",values = c(linear,polynomial,radial,sigmoid)),
  #     makeNumericParam("cost", lower = 1, upper = 2)
  #   )
  # }

  else if(lrn$id=="classif.svm"){
    print("training svm")
    gs <- makeParamSet(
      makeNumericParam("gamma",lower = -1, upper = 1, trafo = function(x) 10^x),
      makeDiscreteParam("kernel",values = c("polynomial")),
      makeNumericParam("cost", lower = -1, upper = 1, trafo = function(x) 10^x),
      makeIntegerParam("degree",lower = 2,upper=2)
    )
  }


  else if (lrn$id=="classif.nnet"){
    print("training svm")
    gs <- makeParamSet(
      makeIntegerParam("size",lower = 3, upper = 3),
      makeIntegerParam("maxit", lower = 10, upper = 100)
    )
  }
  else if (lrn$id=="classif.logreg"){
    getParamSet("classif.logreg")
    print("training logreg")
    gs <- makeParamSet(
      makeLogicalParam("model",default = TRUE)
    )
  }
  return(gs)
}