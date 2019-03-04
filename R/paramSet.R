#'Search space for Hyperparameters
#'Currently supports major learning algorithms


paramSet<-function(lrn)
{#https://mlr.mlr-org.com/articles/tutorial/tune.html
#https://github.com/jakob-r/mlrHyperopt/blob/master/R/getDefaultParSetValues.R
  getParamSet(lrn)
  

  if (lrn$short.name=="boosting"){
    gs <- makeParamSet(
      makeDiscreteParam(id = "coeflearn", default = "Breiman", values = c("Breiman", "Freund", "Zhu")),
      makeNumericParam(id = "mfinal", default = log2(100L/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeIntegerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L)
    )
  }
  
  else if (lrn$short.name=="C50"){
    gs <- makeParamSet(
      makeIntegerParam(id = "trials", lower = 1L, default = 1L, upper = 100L),
      makeLogicalParam(id = "winnow", default = FALSE)
    )
  }
  
  else if (lrn$short.name=="RRF"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, default = expression(floor(p/3)), upper = expression(p)),
      makeNumericParam(id = "coefReg", default = 0.8, lower = 0, upper = 1),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="ada"){
    gs <- makeParamSet(
      makeNumericParam(id = "iter", default = log2(50/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeIntegerParam(id = "maxdepth", default = 10L, lower = 1L, upper = 10L), # mlr recommends 30 as default
      makeNumericParam(id = "nu", default = 0.1, lower = 0.001, upper = 0.5)
    )
  }
  
  else if (lrn$short.name=="blackboost"){
    gs <- makeParamSet(
      makeIntegerParam(id = "maxdepth", default = 2L, lower = 1L, upper = 10L),
      makeNumericParam(id = "mstop", default = log2(100/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10))
    )
  }
  
  else if (lrn$id=="classif.extraTrees"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam(id = "numRandomCuts", default = 1L, lower = 1L, upper = 25L),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.extraTrees"){
    gs <- makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam(id = "numRandomCuts", default = 1L, lower = 1L, upper = 25L),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="ksvm"){
    gs <- makeParamSet(
      makeNumericParam(id = "C",  upper = 10, lower = -5, trafo = function(x) 2^x, default = log2(1)),
      makeNumericParam(id = "sigma",  upper = 15, lower = -15, trafo = function(x) 2^x, default = expression(kernlab::sigest(as.matrix(getTaskData(task, target.extra = TRUE)[["data"]])), scaled = TRUE)),
      keys = "task"
    )
  }
  
  else if (lrn$short.name=="glmboost"){
    gs <- makeParamSet(
      makeNumericParam(id = "mstop", default = log2(100/10), lower = log2(1/10), upper = log2(1000/10), trafo = function(x) floor(2^x * 10)),
      makeNumericParam("nu", lower = 0, upper = 1, default = 0.1)
    )
  }
  
  else if (lrn$short.name=="gbm"){
    gs <- makeParamSet(
      makeNumericParam(id = "n.trees", lower = log2(10/10), upper = log2(1000/10), trafo = function(x) round(2^x * 10), default = log2(500/10)),
      makeIntegerParam(id = "interaction.depth", default = 1L, lower = 1L, upper = 10L),
      makeNumericParam(id = "shrinkage", default = 0.001, lower = 0.001, upper = 0.6),
      makeIntegerParam(id = "n.minobsinnode", default = 10L, lower = 5L, upper = 25L)
    )
  }
  
  else if (lrn$short.name=="rpart"){
    gs <- makeParamSet(
      makeNumericParam(id = "cp", default = log2(0.01), lower = -10, upper = 0, trafo = function(x) 2^x),
      makeIntegerParam(id = "maxdepth", default = 30L, lower = 3L, upper = 30L),
      makeIntegerParam(id = "minbucket", default = 7L, lower = 5L, upper = 50L),
      makeIntegerParam(id = "minsplit", default = 20L, lower = 5L, upper = 50L)
    )
  }
  
  else if (lrn$short.name=="nnet"){
    print("training nnet")
    gs <- makeParamSet(
      makeIntegerParam(id = "size", default = 3L, lower = 1L, upper = 20L),
      makeNumericParam(id = "decay", default = 10^(-5), lower = -5, upper = 1, trafo = function(x) 10^x)
    )
  }
  
  else if (lrn$short.name=="glmnet"){
    gs <- makeParamSet(
      makeNumericParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericParam(id = "lambda", default = log2(1), lower = -10, upper = 3, trafo = function(x) 2^x)
    )
  }
  
  else if (lrn$short.name=="xgboost"){
    gs <- makeParamSet(
      makeNumericParam(id = "nrounds", lower = log2(10/10), upper = log2(4000/10), trafo = function(x) round(2^x * 10), default = log2(10/10)),
      makeIntegerParam(id = "max_depth", default = 6L, lower = 1L, upper = 10L),
      makeNumericParam(id = "eta", default = 0.3, lower = 0.001, upper = 0.6),
      makeNumericParam(id = "gamma", default = 0, lower = 0, upper = 10),
      makeNumericParam(id = "colsample_bytree", default = 0.5, lower = 0.3, upper = 0.7),
      makeNumericParam(id = "min_child_weight", default = 1, lower = 0, upper = 20),
      makeNumericParam(id = "subsample", default = 1, lower = 0.25, upper = 1)
    )
  }
  
  else if (lrn$id=="classif.randomForest"){
    gs <- makeParamSet(
      makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(floor(sqrt(p)))),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.randomForest"){
    gs <- makeParamSet(
      makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
      makeIntegerParam(id = "mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      keys = "p"
    )
  }
  
  else if (lrn$id=="classif.ranger"){
    gs <- makeParamSet(
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(floor(sqrt(p)))),
      makeIntegerParam("min.node.size", lower = 1, upper = 10, default = 1),
      keys = "p"
    )
  }
  
  else if (lrn$id=="regr.ranger"){
    
    gs <- makeParamSet(
      makeIntegerParam("mtry", lower = 1L, upper = expression(p), default = expression(max(floor(p/3), 1))),
      makeIntegerParam("min.node.size", lower = 1, upper = 10, default = 5),
      keys = "p"
    )
  }
  
  else if (lrn$short.name=="svm"){
    gs <- makeParamSet(
      makeNumericParam(id = "cost",  upper = 15, lower = -15, trafo = function(x) 2^x, default = log2(1)),
      makeNumericParam(id = "gamma",  upper = 15, lower = -15, trafo = function(x) 2^x, default = expression(log2(1/p))),
      keys = "p"
    )
  }

  return(gs)
}