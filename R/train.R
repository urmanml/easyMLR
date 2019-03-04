#' Used to train the model
#' input: lrn, task
#' output: trained model( mod)



train<-function(lrn,task)
{
    mod <- mlr::train(lrn, task)
    return(mod)
}