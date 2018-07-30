real_train <-read.csv("train.csv")
real_test1 <-read.csv("test.csv")


real_train$income <- as.numeric(factor(real_train$income, levels=c("Under $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $109,999", "$110,000 to $119,999", "$120,000 to $129,999", "$130,000 to $139,999", "$140,000 to $149,999", "$150,000 to $159,999", "$160,000 to $169,999", "$170,000 to $179,999", "$180,000 to $189,999", "$190,000 to $199,999", "$200,000 to $209,999", "$210,000 to $219,999", "$220,000 to $229,999", "$230,000 to $239,999", "$240,000 to $249,999", "$250,000 to $259,999", "$260,000 to $269,999", "$270,000 to $279,999", "$280,000 to $289,999", "$290,000 to $299,999", "$300,000 & Over")))
real_train$ppark <- as.numeric(factor(real_train$ppark, levels=c("Daily","Weekly","Monthly","Yearly","Never")))
real_train$Urb <- as.numeric(factor(real_train$Urb, levels=c("Rural/Country","Suburban","Urban/City")))
real_train$night <- as.numeric(factor(real_train$night, levels=c("Under 10%","10% To 20%","21% To 30%","31% To 40%","41% To 50%","51% To 60%","61% To 70%","71% To 80%","81% To 90%","91% To 100%")))
real_train$age <- as.numeric(factor(real_train$age, levels=c("Under 30","30 To 39","40 To 49","50 To 59","60 & Over")))
real_train$miles <- as.numeric(factor(real_train$miles, levels=c("Under 50 Miles","51 To 100 Miles","101 To 150 Miles","151 To 200 Miles","201 To 250 Miles","251 To 300 Miles","301 To 350 Miles","351 To 400 Miles","Over 400 Miles")))

###create cross-validation set#####
library(caTools)
set.seed(2000)
spl<-sample.split(real_train$Choice, SplitRatio=0.5)
train_1 <- subset(real_train, spl==TRUE)
train_2 <- subset(real_train, spl==FALSE)

set.seed(2000)
spl<-sample.split(train_1$Choice, SplitRatio=0.5)
train1 <- subset(train_1, spl==TRUE)
train2 <- subset(train_1, spl==FALSE)

set.seed(2000)
spl<-sample.split(train_2$Choice, SplitRatio=0.5)
train3 <- subset(train_2, spl==TRUE)
train4 <- subset(train_2, spl==FALSE)

train_k1 <-rbind(train1,train2,train3)
test_k1 <-train4
train_k2 <-rbind(train1,train2,train4)
test_k2 <-train3
train_k3 <-rbind(train1,train3,train4)
test_k3 <-train2
train_k4 <-rbind(train2,train3,train4)
test_k4 <-train1


TrainList <- list(train_k1,train_k2,train_k3,train_k4)
TestList <- list(test_k1,test_k2,test_k3,test_k4)

library(mlogit)
library(randomForest)

########### Stratified 4-fold valiadation####

avg_logloss <-0
for( i in 1:4)
{train<- TrainList[[i]]
# train <-as.data.frame(train)

S <- mlogit.data(train,shape="wide",choice="Choice",sep="",varying=c(4:83),alt.levels=c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
S$ppincome <- (S$Price)+S$income

M <- mlogit(Choice~BZ+FC+FP+RP+KA+TS+NV+LB+AF+HU+Price|income+ppark+night+age+miles,data=S)
set.seed(1000)
forest <- randomForest(Choice~CC1+GN1+NS1+BU1+FA1+LD1+BZ1+FC1+FP1+RP1+PP1+KA1+SC1+TS1+NV1+MA1+LB1+AF1+HU1+Price1+CC2+GN2+NS2+BU2+FA2+LD2+BZ2+FC2+FP2+RP2+PP2+KA2+SC2+TS2+NV2+MA2+LB2+AF2+HU2+Price2+CC3+GN3+NS3+BU3+FA3+LD3+BZ3+FC3+FP3+RP3+PP3+KA3+SC3+TS3+NV3+MA3+LB3+AF3+HU3+Price3+CC4+GN4+NS4+BU4+FA4+LD4+BZ4+FC4+FP4+RP4+PP4+KA4+SC4+TS4+NV4+MA4+LB4+AF4+HU4+Price4+segment+year+miles+night+gender+age+educ+region+Urb+income+ppark, data=train, ntree=600,mtry=28,nodesize = 25)

test <-TestList[[i]]
# test<-as.data.frame(test)
testdata <- mlogit.data(test,shape="wide",choice="Choice",sep="",varying=c(4:83),alt.levels=c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
testdata$ppincome <-  (testdata$Price)+(testdata$income)


pred_mlogit <- predict(M,newdata=testdata)
pred_forest <- predict(forest, newdata=test, type='prob')

pred <- (1-0.5)*pred_mlogit +(0.5)*pred_forest

choices<- test[,c("Ch1","Ch2","Ch3","Ch4")]
error <- (sum(choices * log(pred)))*(-1/nrow(test))
avg_logloss <- error + avg_logloss
}
f_avg_logloss <-avg_logloss/4
f_avg_logloss


###on actual test data######
real_train <-read.csv("train.csv")
real_test <-read.csv("test1.csv")

library(randomForest)
library(mlogit)
real_train$income <- as.numeric(factor(real_train$income, levels=c("Under $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $109,999", "$110,000 to $119,999", "$120,000 to $129,999", "$130,000 to $139,999", "$140,000 to $149,999", "$150,000 to $159,999", "$160,000 to $169,999", "$170,000 to $179,999", "$180,000 to $189,999", "$190,000 to $199,999", "$200,000 to $209,999", "$210,000 to $219,999", "$220,000 to $229,999", "$230,000 to $239,999", "$240,000 to $249,999", "$250,000 to $259,999", "$260,000 to $269,999", "$270,000 to $279,999", "$280,000 to $289,999", "$290,000 to $299,999", "$300,000 & Over")))
real_train$ppark <- as.numeric(factor(real_train$ppark, levels=c("Daily","Weekly","Monthly","Yearly","Never")))
real_train$Urb <- as.numeric(factor(real_train$Urb, levels=c("Rural/Country","Suburban","Urban/City")))
real_train$night <- as.numeric(factor(real_train$night, levels=c("Under 10%","10% To 20%","21% To 30%","31% To 40%","41% To 50%","51% To 60%","61% To 70%","71% To 80%","81% To 90%","91% To 100%")))
real_train$age <- as.numeric(factor(real_train$age, levels=c("Under 30","30 To 39","40 To 49","50 To 59","60 & Over")))
real_train$miles <- as.numeric(factor(real_train$miles, levels=c("Under 50 Miles","51 To 100 Miles","101 To 150 Miles","151 To 200 Miles","201 To 250 Miles","251 To 300 Miles","301 To 350 Miles","351 To 400 Miles","Over 400 Miles")))

set.seed(1000)
forest <- randomForest(Choice~CC1+GN1+NS1+BU1+FA1+LD1+BZ1+FC1+FP1+RP1+PP1+KA1+SC1+TS1+NV1+MA1+LB1+AF1+HU1+Price1+CC2+GN2+NS2+BU2+FA2+LD2+BZ2+FC2+FP2+RP2+PP2+KA2+SC2+TS2+NV2+MA2+LB2+AF2+HU2+Price2+CC3+GN3+NS3+BU3+FA3+LD3+BZ3+FC3+FP3+RP3+PP3+KA3+SC3+TS3+NV3+MA3+LB3+AF3+HU3+Price3+CC4+GN4+NS4+BU4+FA4+LD4+BZ4+FC4+FP4+RP4+PP4+KA4+SC4+TS4+NV4+MA4+LB4+AF4+HU4+Price4+segment+year+miles+night+gender+age+educ+region+Urb+income+ppark, data=real_train, ntree=600,mtry=21,nodesize=19)
S_master_real <- mlogit.data(real_train,shape="wide",choice="Choice",sep="",varying=c(4:83),alt.levels=c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
S_master_real$ppincome <- (S_master_real$income)+(S_master_real$Price)

M_master_real <- mlogit(Choice~CC+GN+NS+BU+FA+LD+BZ+FC+FP+RP+PP+KA+SC+TS+NV+MA+LB+AF+HU+ppincome|ppark+Urb+night+age+miles,data=S_master_real)

real_test$income <- as.numeric(factor(real_test$income, levels=c("Under $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $109,999", "$110,000 to $119,999", "$120,000 to $129,999", "$130,000 to $139,999", "$140,000 to $149,999", "$150,000 to $159,999", "$160,000 to $169,999", "$170,000 to $179,999", "$180,000 to $189,999", "$190,000 to $199,999", "$200,000 to $209,999", "$210,000 to $219,999", "$220,000 to $229,999", "$230,000 to $239,999", "$240,000 to $249,999", "$250,000 to $259,999", "$260,000 to $269,999", "$270,000 to $279,999", "$280,000 to $289,999", "$290,000 to $299,999", "$300,000 & Over")))
real_test$ppark <- as.numeric(factor(real_test$ppark, levels=c("Daily","Weekly","Monthly","Yearly","Never")))
real_test$Urb <- as.numeric(factor(real_test$Urb, levels=c("Rural/Country","Suburban","Urban/City")))
real_test$night <- as.numeric(factor(real_test$night, levels=c("Under 10%","10% To 20%","21% To 30%","31% To 40%","41% To 50%","51% To 60%","61% To 70%","71% To 80%","81% To 90%","91% To 100%")))
real_test$age <- as.numeric(factor(real_test$age, levels=c("Under 30","30 To 39","40 To 49","50 To 59","60 & Over")))
real_test$miles <- as.numeric(factor(real_test$miles, levels=c("Under 50 Miles","51 To 100 Miles","101 To 150 Miles","151 To 200 Miles","201 To 250 Miles","251 To 300 Miles","301 To 350 Miles","351 To 400 Miles","Over 400 Miles")))

pred_forest <- predict(forest, newdata=real_test, type='prob')

testdata_real <- mlogit.data(real_test,shape="wide",choice="Choice",sep="",varying=c(4:83),alt.levels=c("Ch1","Ch2","Ch3","Ch4"),id.var="Case")
testdata_real$ppincome <- (testdata_real$income)+(testdata_real$Price)
pred_mlogit <- predict(M_master_real,newdata=testdata_real)

pred_real<- (1-0.5)*pred_mlogit+0.5*pred_forest

realtestdata <- as.data.frame(pred_real)
write.csv(realtestdata, "output13.csv")

