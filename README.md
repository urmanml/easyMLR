# easyMLR
End to End Machine Learning Framework in simple English language. It is created using a high level abstraction layer over Popular MLR package. 

## Benefits:
It has following benefits-
1)	Faster time to development 
2)	A standard framework for solving machine Learning problems-
3)	Low learning curve. Even a business person having no knowledge of Machine Learning can implement and solve a Machine Learning problem in less than a less than two hours

Components:
-----------

It has all the components required for end to end Machine Learning workflow-

##### 1) Data Preparation-

1.  Reading the dataset

2.  Missing value imputation

3.  Outlier Treatment

4.  Feature Engineering

5.  Feature Selection

6.  Removing Class imbalance

##### 2) Deciding Learning Algorithm

1.  Hyperparameter Tuning: By calling a simple function, it can tune set of relevant hyperparameters using crossvalidation

2.  Benchmamrking: It can benchmark different learning algorithms to pick the best algorithm for the dataset as per a supplied metric

3.  Analyze performance vs  Thresholds: Generating graph of the model performance for different values of thresholds for different learning algorithms to select best learning algorithm and the threshold in case of Classification Problems

##### 3) Model Training and Prediction

1.  Model Training

2.  Prediction using trained model  

##### 4) Model Evaluation

1.  Performance evaluation: Generate summary of model performance as per different metrics like accuracy, precision, recall

## Installing
```
install.packages("devtools")
library(devtools)
install_github("urmanml/easyMLR")
```
Sample Code
---------------------------------------
```
library(easymlr)
```
0) Load data
```
dataset<-read.csv("loan_data.csv")
colnames<-colnames(dataset)
target<-colnames[2]
```


1) Data Preprocessing 
```
dataset<-missingValueImputation(dataset,target)
dataset<-outlierTreatment(dataset,target)
dataset<-featureEngineering(dataset)
dataset<-featureNormalization(dataset,target)
```
1b) Specify problem type here-"classif","regr","cluster"
```
task<-createTask(dataset,target,type = "classif")
lrns<-createLearnerList(task$type)
#task<-featureSelection(task)
task<-treatClassImbalance(task)
lrns<-tuneLearners(lrns,task)
```
2a) Benchmark Algorithms using Cross Validation
```
bmr<-benchmarkExperiment(lrns,task)
analyseThresholdVsPerformance(bmr)
```
2b) Analyze output and select an Algorithm
```
lrn<-selectLearner(lrns,name = "gbm")
```
3) Model Training and Prediction
```
mod<-train(lrn,task = task)
pred<-prediction(mod,task,.29)
```
4) Performance Evaluation
```
easymlr::performance(pred)
```
