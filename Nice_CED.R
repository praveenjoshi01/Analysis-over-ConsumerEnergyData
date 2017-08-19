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
str(ced_data)
levels(ced_data$education)
ced_data$education<-as.factor(as.character(tolower(ced_data$education))) 
levels(ced_data$education)
levels(ced_data$modeOfPayment)
ced_data$modeOfPayment<- as.factor(as.character(gsub('\\s','',ced_data$modeOfPayment )))
levels(ced_data$modeOfPayment)
levels(ced_data$typeOfHousing)
ced_data$typeOfHousing <- ifelse(ced_data$typeOfHousing=="1BHK-Apt", 1, 
                                 ifelse(ced_data$typeOfHousing=="2BHK-Apt", 2,
                                        ifelse(ced_data$typeOfHousing=="3BHK-Apt", 3,
                                               ifelse(ced_data$typeOfHousing=="5BHK-House", 4,
                                                      ifelse(ced_data$typeOfHousing=="Mansion", 5,NA)))) )

ced_data$typeOfHousing <- as.factor(as.character(as.numeric(ced_data$typeOfHousing)))
levels(ced_data$typeOfHousing)
levels(ced_data$ageOfBuilding)
ced_data$ageOfBuilding <- ifelse(ced_data$ageOfBuilding=="<10yrs", 1, 
                                 ifelse(ced_data$ageOfBuilding=="10-20yrs", 2,
                                        ifelse(ced_data$ageOfBuilding=="20-30yrs", 3,
                                               ifelse(ced_data$ageOfBuilding==">30yrs", 4,NA))) )

ced_data$ageOfBuilding <- as.factor(as.character(as.numeric(ced_data$ageOfBuilding)))
levels(ced_data$ageOfBuilding)
levels(ced_data$ageOfCustomerAccountInDays)
ced_data$ageOfCustomerAccountInDays<- as.factor(as.character(gsub('\\s','',ced_data$ageOfCustomerAccountInDays )))
ced_data$ageOfCustomerAccountInDays <- ifelse(ced_data$ageOfCustomerAccountInDays=="<1yr", 1, 
                                              ifelse(ced_data$ageOfCustomerAccountInDays=="1-2yrs", 12,
                                                     ifelse(ced_data$ageOfCustomerAccountInDays=="2-5yrs", 25,
                                                            ifelse(ced_data$ageOfCustomerAccountInDays==">5yrs", 510,
                                                                   ifelse(ced_data$ageOfCustomerAccountInDays==">10yrs",10,NA)))) )

ced_data$ageOfCustomerAccountInDays <- as.factor(as.character(as.numeric(ced_data$ageOfCustomerAccountInDays)))

levels(ced_data$ageOfCustomerAccountInDays)

str(ced_data)
num_attr = c("age", "freqOfMoving", "lateFeePayments", "AvgComplaintsRaised", "AvgComplaintsResolved")
cat_attr = setdiff(x = names(ced_data), y = c(num_attr, "Target"))

ced_data[,num_attr] <- scale(ced_data[,num_attr])


library(caret)
set.seed(124)
train_rows = createDataPartition(ced_data$Target, p=0.7,list= F)
prop.table(table(ced_data$Target))
train_data = ced_data[train_rows,]
prop.table(table(train_data$Target))
test_data = ced_data[-train_rows, ,-c(17)]
test_pred = ced_data[-train_rows,c(17)]

######################################## Model ########################################

multinom_model12 = multinom(Target~ age*familySize*avgDailyUsage*lateFeePayments
                            *education*AvgComplaintsResolved*ageOfCustomerAccountInDays
                            , data = train_data,cor =T )

pred_results = multinom_model12$fitted.values

pred_train  = prediction(pred_results, train_data$Target)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)
plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]

################################# Predict Test ########################################

prb_train =predict(multinom_model12,test_data,type = "probs")

library(ROCR)
pred_train  = prediction(prb_train,test_pred)

perf = performance(pred_train, measure = "tpr", x.measure = "fpr")
plot(perf)

plot(perf, col =rainbow(10), print.cutoffs.at= seq(0,1,0.01) , colorize=T)

perf_auc = performance(pred_train, measure = "auc")
perf_auc@y.values[[1]]
