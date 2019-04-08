set.seed(100)
library(mlr)


income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")



# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)

#   age: continuous.

#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.

#   fnlwgt: continuous.

#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.

#   education-num: continuous.

#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.

#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.

#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.

#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.

#   sex: Female, Male.

#   capital-gain: continuous.

#   capital-loss: continuous.

#   hours-per-week: continuous.

#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.



######################

# Clean up the data

######################

# Drop unnecessary columns

income$native.country <- NULL

income$fnlwgt         <- NULL

# Make sure continuous variables are coded as such

income$age            <- as.numeric(income$age)

income$hours          <- as.numeric(income$hours)

income$education.num  <- as.numeric(income$education.num)

income$capital.gain   <- as.numeric(income$capital.gain)

income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels

levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))

levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))

levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))

levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))

levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))



# Break up the data:
income$dummy <- ifelse(income$high.earner==">50K", 1, 0)
n <- nrow(income)

train <- sample(n, size = .8*n)

test  <- setdiff(1:n, train)

income.train <- income[train,]

income.test  <- income[test, ]

library(rpart)
library(mlr)
library(tidyverse)
library(magrittr)
library(kknn)
library(e1071)
library(rpart)
library(nnet)


# Define the task:
#create a task
trainTask <- makeClassifTask(data = income.train,target = "high.earner")
testTask <- makeClassifTask(data = income.test, target = "high.earner")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

# Take 10 random guesses
tuneMethod <- makeTuneControlRandom(maxit = 10L)



### LEARNERS ###

#make tree learner
tree <- makeLearner("classif.rpart", predict.type = "response")

#logit
logit <- makeLearner("classif.glmnet",predict.type = "response")

#make neural learner
neural <- makeLearner("classif.nnet", predict.type = "response")

#make NB learner
NB <- makeLearner("classif.naiveBayes", predict.type = "response")

#make kknn learner
knnl <- makeLearner("classif.kknn", predict.type = "response")

#make svm learner
svm <- makeLearner("classif.svm", predict.type = "response")

### HYPERPARAMETERS ###

#Tree model
gs_tree <- makeParamSet(
  makeIntegerParam("minsplit",lower=10,upper=50),
  makeIntegerParam("minbucket",lower=5,upper=50),
  makeNumericParam("cp", lower=0.001,upper=0.2)
)

#Logit
gs_logit <- makeParamSet(
  makeNumericParam("lambda",lower=0,upper=3),
  makeNumericParam("alpha",lower=0,upper=1)
  )

#Neural Network
gs_neural <- makeParamSet(
  makeIntegerParam("size" ,lower=1,upper=10),
  makeNumericParam("decay",lower=0.1,upper=0.5),
  makeIntegerParam("maxit",lower=1000,upper=1000)
)

#Naive Bayes
#NOTHING TO DO

#KNN
gs_knn <- makeParamSet(
  makeIntegerParam("k",lower=1,upper=30)
)

#SVM
gs_svm <- makeParamSet(
  makeDiscreteParam("kernel", values = "radial"),
  makeDiscreteParam("cost", values = 2^c(-2,-1,0, 1,2,10)), #cost parameters
  makeDiscreteParam("gamma", values = 2^c(-2,-1,0, 1,2,10)) #RBF Kernel Parameter
)

### TUNING THE MODELS ###

#Tree
Tune_Tree <- tuneParams(learner = tree, 
                           task = trainTask, 
                           resampling = set_cv, 
                           par.set = gs_tree, 
                           control = tuneMethod, 
                           measures=list(f1, gmean))

#Logit
Tune_logit <- tuneParams(learner = logit,
                           task = trainTask,
                           resampling = set_cv,
                           measures=list(f1, gmean),      
                           par.set = gs_logit,
                           control = tuneMethod,
                           show.info = TRUE)



#Neural Network
Tune_NN <- tuneParams(learner = neural,
                            task = trainTask,
                            resampling = set_cv,
                            measures=list(f1, gmean),      
                            par.set = gs_neural,
                            control = tuneMethod,
                            show.info = TRUE)


#KNN
Tune_KNN <- tuneParams(learner = knnl,
                         task = trainTask,
                         resampling = set_cv,
                         measures=list(f1, gmean),      
                         par.set = gs_knn,
                         control = tuneMethod,
                         show.info = TRUE)



#SVM
Tune_SVM <- tuneParams(learner = svm,
                         task = trainTask,
                         resampling = set_cv,
                         measures=list(f1, gmean),      
                         par.set = gs_svm,
                         control = tuneMethod,
                         show.info = TRUE)



# Apply the optimal algorithm parameters to the model
pred_tree <- setHyperPars(learner=tree, par.vals = Tune_Tree$x)
pred_logit <- setHyperPars(learner=logit, par.vals = Tune_logit$x)
pred_neural <- setHyperPars(learner=neural, par.vals = Tune_NN$x)
pred_knn <- setHyperPars(learner=knnl, par.vals = Tune_KNN$x)
pred_svm <- setHyperPars(learner=svm, par.vals = Tune_SVM$x)
pred_nb <- setHyperPars(learner=NB)


# Verify performance on cross validated sample sets
resample(pred_tree,trainTask,resampleStrat,measures=list(f1, gmean))
resample(pred_logit,trainTask,resampleStrat,measures=list(f1, gmean))
resample(pred_neural,trainTask,resampleStrat,measures=list(f1, gmean))
resample(pred_knn,trainTask,resampleStrat,measures=list(f1, gmean))
resample(pred_svm,trainTask,resampleStrat,measures=list(f1, gmean))
resample(pred_nb,trainTask,resampleStrat,measures=list(f1, gmean))


# Train the final model
finalModel_tree <- train(learner = pred_tree, task = trainTask)
finalModel_logit <- train(learner = pred_logit, task = trainTask)
finalModel_neural <- train(learner = pred_neural, task = trainTask)
finalModel_knn <- train(learner = pred_knn, task = trainTask)
finalModel_svm <- train(learner = pred_svm, task = trainTask)
finalModel_NB <- train(learner = pred_nb, task = trainTask)


# Predict in test set!
prediction_tree <- predict(finalModel_tree, newdata = income.test)
prediction_logit <- predict(finalModel_logit, newdata = income.test)
prediction_neural <- predict(finalModel_neural, newdata = income.test)
prediction_knn <- predict(finalModel_knn, newdata = income.test)
prediction_svm <- predict(finalModel_svm, newdata = income.test) 
prediction_NB <- predict(finalModel_NB, newdata = income.test) 



print(Tune_Tree)
print(Tune_logit)
print(Tune_NN)
print(Tune_KNN)
print(Tune_SVM)



   


