# easyMLR
End to End Machine Learning Framework in simple English language. It is created using a high level abstraction layer over Popular MLR package. It has following benefits-

1)	Faster time to development 
2)	A standard framework for solving machine Learning problems-
3)	Low learning curve. Even a business person having no knowledge of Machine Learning can implement and solve a Machine Learning problem in less than a less than two hours

It has all the components required for end to end Machine Learning workflow-
1)	Data Preparation-
  a)	Reading the dataset
  b)	Missing value imputation
  c)	Outlier Treatment
  d)	Feature Engineering
  e)	Feature Selection
  f)	Removing Class imbalance

2)	Deciding Learning Algorithm
  a)	Hyperparameter Tuning: By calling a simple function, it can tune set of relevant hyperparameters using crossvalidation
  b)	Benchmamrking: It can benchmark different learning algorithms to pick the best algorithm for the dataset as per a supplied metric
  c)	Analyze performance vs  Thresholds: Generating graph of the model performance for different values of thresholds for different learning algorithms to select best learning algorithm and the threshold in case of Classification Problems

3)	Model Training and Prediction
  a)	Model Training
  b)	Prediction using trained model	

4)	Model Evaluation
  a)	Performance evaluation: Generate summary of model performance as per different metrics like accuracy, precision, recall



Sample Code
---------------------------------------
library(easymlr)

####0) Load data
dataset<-read.csv("loan_data.csv")
colnames<-colnames(dataset)
target<-colnames[2]
#--------------------------------------
####1) Data Preprocessing 
dataset<-missingValueImputation(dataset,target)
dataset<-outlierTreatment(dataset,target)
dataset<-featureEngineering(dataset)
dataset<-featureNormalization(dataset,target)
#--------------------------------------
####1b) Specify problem type here-"classif","regr","cluster"
task<-createTask(dataset,target,type = "classif")
lrns<-createLearnerList(task$type)
#task<-featureSelection(task)
task<-treatClassImbalance(task)
lrns<-tuneLearners(lrns,task)
#--------------------------------------
####2a) Benchmark Algorithms using Cross Validation
bmr<-benchmarkExperiment(lrns,task)
analyseThresholdVsPerformance(bmr)
#--------------------------------------
####2b) Analyse output and select an Algorithm
lrn<-selectLearner(lrns,name = "gbm")
#--------------------------------------
####3) Model Training and Prediction
mod<-train(lrn,task = task)
pred<-prediction(mod,task,.29)
#--------------------------------------
####4) Performance Evaluation
easymlr::performance(pred)
#--------------------------------------
