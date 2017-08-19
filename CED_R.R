rm(list = ls(all=TRUE))
setwd("C:\\Users\\prave\\Desktop\\Cute02\\CaseStudies\\Problem1")
ced_data <- read.csv("ConsumerEnergyData.csv")
untouched_ced_data <- read.csv("ConsumerEnergyData.csv")


summary(ced_data)
str(ced_data)
#Converting to integer to numeric
ced_data$age = as.numeric(ced_data$age)
ced_data$freqOfMoving= as.numeric(ced_data$freqOfMoving)
ced_data$lateFeePayments= as.numeric(ced_data$lateFeePayments)
ced_data$AvgComplaintsRaised = as.numeric(ced_data$AvgComplaintsRaised)
ced_data$AvgComplaintsResolved=as.numeric(ced_data$AvgComplaintsResolved)
ced_data$DiffOfComplaintsRaisedAndResolved
str(ced_data)
levels(ced_data$education)
ced_data$education<-as.factor(as.character(tolower(ced_data$education))) 
levels(ced_data$education)
levels(ced_data$modeOfPayment)
ced_data$modeOfPayment<- as.factor(as.character(gsub('\\s','',ced_data$modeOfPayment )))
levels(ced_data$modeOfPayment)
levels(ced_data$typeOfHousing)
ced_data$typeOfHousing <- as.factor(as.character(as.numeric(ced_data$typeOfHousing)))
levels(ced_data$typeOfHousing)
levels(ced_data$ageOfBuilding)
ced_data$ageOfBuilding <- as.factor(as.character(as.numeric(ced_data$ageOfBuilding)))
levels(ced_data$ageOfBuilding)
levels(ced_data$ageOfCustomerAccountInDays)
ced_data$ageOfCustomerAccountInDays<- as.factor(as.character(gsub('\\s','',ced_data$ageOfCustomerAccountInDays )))
ced_data$ageOfCustomerAccountInDays <- as.factor(as.character(as.numeric(ced_data$ageOfCustomerAccountInDays)))
ced_data$DiffRaisedToResolved <- ced_data$AvgComplaintsRaised -ced_data$AvgComplaintsResolved

levels(ced_data$ageOfCustomerAccountInDays)
library(caret)
NoVariationOrNoChangeInData<-nearZeroVar(ced_data, saveMetrics= TRUE)
NoVariationOrNoChangeInData

sum(is.na(ced_data))
str(ced_data)
#library(infotheo)
#ced_data$ageOfBuilding <-as.vector(discretize(ced_data$ageOfBuilding, disc="equalwidth",nbins=4))
#ced_data$ageOfCustomerAccountInDays <- as.vector(discretize(ced_data$ageOfCustomerAccountInDays, disc="equalwidth",nbins=7))

#dumm

library(dummies)
dummyAttr = c("education","familySize","typeOfHousing","houseOwnership","householdAnnualIncome","ageOfBuilding","modeOfPayment","typeOfPlanI","typeOfPlanII")
#nonDummy_Attr <- setdiff(x = names(ced_data),y = dummyAttr)

subsetOfFactors<-ced_data[,dummyAttr]

DummyVars<-dummy.data.frame(subsetOfFactors)
str(DummyVars)
DummyVars<- as.data.frame(sapply(DummyVars, as.numeric))

Data <- cbind(ced_data,DummyVars)
Data<-Data[,!(names(Data) %in% dummyAttr)]

str(Data)

#new glm
logReg = glm(Target ~ ., data = Data, family = "binomial")
summary(logReg)
library(MASS)
logReg_step= stepAIC(logReg,direction = "both")
summary(logReg_step)
library(car)
logReg_step_vif = vif(logReg_step)
logReg_step_vif

prb_train =predict(logReg_step,type = "response")
library(ROCR)
pred_train  = prediction(prb_train, Data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)

plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]
#######################applying glm on base DataSet####################
#new glm
logReg1 = glm(Target ~ ., data = ced_data, family = "binomial")

library(MASS)
logReg1_step= stepAIC(logReg1,direction = "both",scale = T,)
summary(logReg1_step)

prb_train =predict(logReg1_step,type = "response")

library(ROCR)
pred_train  = prediction(prb_train, ced_data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)

plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]

#######################applying glm on untouched base DataSet####################
#new glm
logReg2 = glm(Target ~ ., data = untouched_ced_data, family = "binomial")

library(MASS)
logReg2_step= stepAIC(logReg2,direction = "both")
summary(logReg2_step)

prb_train =predict(logReg2_step,type = "response")
library(ROCR)
pred_train  = prediction(prb_train,untouched_ced_data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)

plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]

##multinom

library(nnet)
##Data df
multinom_model = multinom(Target~ ., data = Data )

pred_results = multinom_model$fitted.values

pred_train  = prediction(pred_results, Data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)
plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]

## ced_data df
multinom_model1 = multinom(Target~ ., data = ced_data,cor =T )

pred_results = multinom_model1$fitted.values

pred_train  = prediction(pred_results, ced_data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)
plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]


##Data df22
multinom_model22 = multinom(Target~ age*avgDailyUsage*lateFeePayments
                            *educationhigh*AvgComplaintsResolved*ageOfCustomerAccountInDays
                            , data = Data )

pred_results = multinom_model22$fitted.values

pred_train  = prediction(pred_results, Data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)
plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]


## ced_data df2
multinom_model12 = multinom(Target~ age*familySize*avgDailyUsage*lateFeePayments
                           *education*DiffRaisedToResolved*ageOfCustomerAccountInDays
                           , data = ced_data,cor =T )

pred_results = multinom_model12$fitted.values

pred_train  = prediction(pred_results, ced_data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)
plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]


###############################H2o###########################################

# Divide the data in to test and train
rows=seq(1,nrow(ced_data),1)
set.seed(123)
trainRows = sample(rows, nrow(ced_data)*.8)
set.seed(123)
testRows=rows[-(trainRows)]

train = ced_data[trainRows,] 
test = ced_data[testRows,] 

rm(ced_data, rows, testRows, trainRows)

# Load H2o library
library(h2o)

# Start H2O on the local machine using all available cores and with 2 gigabytes of memory
h2o.init(nthreads = -1, max_mem_size = "2g")

# Import a local R train data frame to the H2O cloud
train.hex <- as.h2o(x = train, destination_frame = "train.hex")

# Lambda search
model_LS = h2o.glm(y = "Target", 
                   x = setdiff(names(train.hex), "Train"),
                   training_frame = train.hex, 
                   family = "binomial",
                   lambda_search = TRUE)

print(model_LS)

test.hex <- as.h2o(x = test, destination_frame = "test.hex")
# Predict on same training data set
predict.hex = h2o.predict(model_LS, 
                          newdata = test.hex[,setdiff(names(test.hex), "Target")])

data_GLM = h2o.cbind(test.hex[,"Target"], predict.hex)

# Copy predictions from H2O to R
pred_GLM = as.data.frame(data_GLM)

conf_Matrix_GLM = table(pred_GLM$Target, pred_GLM$predict) 

Accuracy = (conf_Matrix_GLM[1,1]+conf_Matrix_GLM[2,2])/sum(conf_Matrix_GLM)


# Prepare the parameters for the for H2O glm grid search
lambda_opts = list(list(1), list(.5), list(.1), list(.01), 
                   list(.001), list(.0001), list(.00001), list(0))
alpha_opts = list(list(0), list(.25), list(.5), list(.75), list(1))

hyper_parameters = list(lambda = lambda_opts, alpha = alpha_opts)

# Build H2O GLM with grid search
grid_GLM <- h2o.grid("glm", 
                     hyper_params = hyper_parameters, 
                     grid_id = "grid_GLM.hex",
                     y = "Target", 
                     x = setdiff(names(train.hex), "Target"),
                     training_frame = train.hex, 
                     family = "binomial")

# Remove unused R objects
rm(lambda_opts, alpha_opts, hyper_parameters)

# Get grid summary
summary(grid_GLM)

# Fetch GBM grid models
grid_GLM_models <- lapply(grid_GLM@model_ids, 
                          function(model_id) { h2o.getModel(model_id) })

for (i in 1:length(grid_GLM_models)) 
{ 
  print(sprintf("regularization: %-50s auc: %f", grid_GLM_models[[i]]@model$model_summary$regularization, h2o.auc(grid_GLM_models[[i]])))
}

# Function to find the best model with respective to AUC
find_Best_Model <- function(grid_models){
  best_model = grid_models[[1]]
  best_model_AUC = h2o.auc(best_model)
  for (i in 2:length(grid_models)) 
  {
    temp_model = grid_models[[i]]
    temp_model_AUC = h2o.auc(temp_model)
    if(best_model_AUC < temp_model_AUC)
    {
      best_model = temp_model
      best_model_AUC = temp_model_AUC
    }
  }
  return(best_model)
}

# Find the best model by calling find_Best_Model Function
best_GLM_model = find_Best_Model(grid_GLM_models)

rm(grid_GLM_models)

# Get the auc of the best GBM model
best_GLM_model_AUC = h2o.auc(best_GLM_model)

# Examine the performance of the best model
best_GLM_model

# View the specified parameters of the best model
best_GLM_model@parameters

# Important Variables.
h2o.varimp(best_GLM_model)

# Import a local R test data frame to the H2O cloud
test.hex <- as.h2o(x = test, destination_frame = "test.hex")


# Predict on same training data set
predict.hex = h2o.predict(best_GLM_model, 
                          newdata = test.hex[,setdiff(names(test.hex), "Target")])

data_GLM = h2o.cbind(test.hex[,"Target"], predict.hex)

# Copy predictions from H2O to R
pred_GLM = as.data.frame(data_GLM)

# Shutdown H2O
h2o.shutdown(F)

# Hit Rate and Penetration calculation
conf_Matrix_GLM = table(pred_GLM$Target, pred_GLM$predict) 

Accuracy = (conf_Matrix_GLM[1,1]+conf_Matrix_GLM[2,2])/sum(conf_Matrix_GLM)

rm(list=ls())
