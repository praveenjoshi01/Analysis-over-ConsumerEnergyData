rm(list = ls(all=TRUE))
setwd("C:\\Users\\prave\\Desktop\\Cute02\\CaseStudies\\Problem1")
ced_data <- read.csv("ConsumerEnergyData.csv")
untouched_ced_data <- read.csv("ConsumerEnergyData.csv")

summary(ced_data)
str(ced_data)
ced_data$age = as.numeric(ced_data$age)
ced_data$freqOfMoving= as.numeric(ced_data$freqOfMoving)
ced_data$lateFeePayments= as.numeric(ced_data$lateFeePayments)
ced_data$AvgComplaintsRaised = as.numeric(ced_data$AvgComplaintsRaised)
ced_data$AvgComplaintsResolved=as.numeric(ced_data$AvgComplaintsResolved)
#ced_data$freqOfMoving<- as.factor((as.character(ced_data$freqOfMoving)))
#ced_data$AvgComplaintsResolved<- as.factor((as.character(ced_data$AvgComplaintsResolved)))
ced_data$education<-as.factor(as.character(tolower(ced_data$education))) 
#maybe mode of payment S D needs treatment. S D -> SD : remove space bw S D
ced_data$modeOfPayment<- as.factor(as.character(gsub('\\s','',ced_data$modeOfPayment )))
sum(is.na(ced_data))

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
logReg1_step= stepAIC(logReg1,direction = "both")
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

