setwd("C:/Users/Yoonhee.YOONHEE/Desktop/공모전/문화관광/데이터")
data2 <- read.csv('data2.csv')
consistency <- data2$Q7A1+data2$Q7A2/12
data2 <- cbind(data2,consistency)
rest_time <- data2$Q13A1A1*5+data2$Q13A1A2*2
data2 <- cbind(data2,rest_time)
data2 <- data2[,-c(12,13,15:19)]
head(data2)

data <- data2
data$consistency <- round(data$consistency,2)
head(data)


#data cleansinsing
data$Q43[is.na(data$Q43)]<-0
data$Q14A1[is.na(data$Q14A1)]<-0
data$consistency[is.na(data$consistency)]<-0
sum(is.na(data))
point <- data$Q9/((data$rest_time)*4)
data <- cbind(data,point)
head(data)
data <- data[,-c(1,11,12,20)]


#random forest
library(randomForest)
data <- data[,-1]
data$SQ1 <- as.character(data$SQ1)
data$GUMO <- as.character(data$GUMO)
data$GENDER <- as.character(data$GENDER)
data$AGE_G1 <- as.character(data$AGE_G1)
data$MER <- as.character(data$MER)
data$INC <- as.character(data$INC)
data$Q37 <- as.character(data$Q37)
data$Q22 <- as.character(data$Q22)
data$Q23 <- as.character(data$Q23)
data$Q30 <- as.character(data$Q30)
data$Q31 <- as.character(data$Q31)
data$Q33 <- as.character(data$Q33)

#data_yb random forest
data_yb <- data[data$AGE_G1==1|data$AGE_G1==2,]
head(data_yb)

model_yb <- randomForest(point~., data=data_yb, importance=T)
importance(model_yb)
varImpPlot(model_yb)

#data_yb decision tree
install.packages("caret")
library(caret)
set.seed(1234)

data_yb <- data_yb[,-4]
sample_numyb <-  sample(1:nrow(data_yb), size = round(0.7 * nrow(data_yb)))
train_data_yb <- data_yb[sample_numyb, ]
test_data_yb <- data_yb[-sample_numyb,]

head(train_data_yb)
library(rpart)
library(party)

train_data_yb$INC <- as.factor(train_data_yb$INC)
train_data_yb$Q37 <- as.factor(train_data_yb$Q37)
train_data_yb$Q43 <- as.factor(train_data_yb$Q43)
train_data_yb$Q14 <- as.factor(train_data_yb$Q14)
train_data_yb$Q22 <- as.factor(train_data_yb$Q22)
train_data_yb$Q23 <- as.factor(train_data_yb$Q23)
train_data_yb$Q30 <- as.factor(train_data_yb$Q30)
train_data_yb$Q31 <- as.factor(train_data_yb$Q31)
train_data_yb$Q33 <- as.factor(train_data_yb$Q33)
train_data_yb$Q45A1 <- as.factor(train_data_yb$Q45A1)

summary(train_data_yb)

tree_yb1 <- ctree(point~ Q43+consistency+Q14A1 ,data=train_data_yb)
plot(tree_yb1)


train_data_yb$SQ1 <- as.factor(train_data_yb$SQ1)
train_data_yb$GUMO <- as.factor(train_data_yb$GUMO)
train_data_yb$GENDER <- as.factor(train_data_yb$GENDER)
train_data_yb$MER <- as.factor(train_data_yb$MER)
tree_yb2 <- ctree(point~. ,data=train_data_yb)

plot(tree_yb2)
prp(rxAddInheritance(tree_yb2))

#data_yb error rate
test_data_yb$INC <- as.factor(test_data_yb$INC)
test_data_yb$Q37 <- as.factor(test_data_yb$Q37)
test_data_yb$Q43 <- as.factor(test_data_yb$Q43)
test_data_yb$Q22 <- as.factor(test_data_yb$Q22)
test_data_yb$Q23 <- as.factor(test_data_yb$Q23)
test_data_yb$Q30 <- as.factor(test_data_yb$Q30)
test_data_yb$Q31 <- as.factor(test_data_yb$Q31)
test_data_yb$Q33 <- as.factor(test_data_yb$Q33)
test_data_yb$Q45A1 <- as.factor(test_data_yb$Q45A1)


treepred <- predict(tree_yb, test_data_yb)
table(treepred, test_data_yb$point)

levels(train_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q32) <- c(1,2,3,4,5,6,7)

summary(train_data_yb)
summary(test_data_yb)
levels(train_data_yb$Q33)
levels(test_data_yb$Q33) <- c(1,2,3,4,5,6,7,8,9,10)
levels(test_data_yb$Q43)<- levels(train_data_yb$Q43)
levels(test_data_yb$Q14)<- levels(train_data_yb$Q14)
levels(test_data_yb$Q37)<- levels(train_data_yb$Q37)

library(rpart)
tree_yb_error <- rpart(point~Q43+consistency+Q14A1,data=train_data_yb)
printcp(tree_yb_error)

#data_mb random forest
data_mb <- data[data$AGE_G1==3|data$AGE_G1==4|data$AGE_G1==5|data$AGE_G1==6,]
head(data_mb)

model_mb <- randomForest(point~, data=data_mb, importance=T, na.action = na.omit)
importance(model_mb)
varImpPlot(model_mb)

#data_mb decision tree
library(caret)
set.seed(1234)

data_mb <- data_mb[,-4]
sample_nummb <-  sample(1:nrow(data_mb), size = round(0.7 * nrow(data_mb)))
sample_nummb
train_data_mb <- data_yb[sample_nummb, ]
test_data_mb <- data_yb[-sample_nummb,]

library(rpart)
library(party)

head(train_data_mb)

train_data_mb$INC <- as.factor(train_data_mb$INC)
train_data_mb$Q37 <- as.factor(train_data_mb$Q37)
train_data_mb$Q43 <- as.factor(train_data_mb$Q43)
train_data_mb$Q14 <- as.factor(train_data_mb$Q14)
train_data_mb$Q22 <- as.factor(train_data_mb$Q22)
train_data_mb$Q23 <- as.factor(train_data_mb$Q23)
train_data_mb$Q30 <- as.factor(train_data_mb$Q30)
train_data_mb$Q31 <- as.factor(train_data_mb$Q31)
train_data_mb$Q33 <- as.factor(train_data_mb$Q33)
train_data_mb$Q45A1 <- as.factor(train_data_mb$Q45A1)
train_data_mb$SQ1 <- as.factor(train_data_mb$SQ1)
summary(train_data_mb)
sum(is.na(train_data_mb))
train_data_mb <- na.omit(train_data_mb)
tree_mb1 <- ctree(point~ Q43+SQ1+Q45A1 ,data=train_data_mb, )
plot(tree_mb1)


#data_ob random forest
data_ob <- data[data$AGE_G1==7,]
head(data_ob)

model_ob <- randomForest(point~., data=data_ob, importance=T, na.action = na.omit)
importance(model_ob)
varImpPlot(model_ob)

#data_ob decision tree
library(caret)
set.seed(1234)

data_ob <- data_ob[,-4]
sample_numob <-  sample(1:nrow(data_ob), size = round(0.7 * nrow(data_ob)))
sample_numob
train_data_ob <- data_yb[sample_numob, ]
test_data_ob <- data_yb[-sample_numob,]

library(rpart)
library(party)

head(train_data_ob)

train_data_ob$INC <- as.factor(train_data_ob$INC)
train_data_ob$Q37 <- as.factor(train_data_ob$Q37)
train_data_ob$Q43 <- as.factor(train_data_ob$Q43)
train_data_ob$Q14 <- as.factor(train_data_ob$Q14)
train_data_ob$Q22 <- as.factor(train_data_ob$Q22)
train_data_ob$Q23 <- as.factor(train_data_ob$Q23)
train_data_ob$Q30 <- as.factor(train_data_ob$Q30)
train_data_ob$Q31 <- as.factor(train_data_ob$Q31)
train_data_ob$Q33 <- as.factor(train_data_ob$Q33)
train_data_ob$Q45A1 <- as.factor(train_data_ob$Q45A1)
train_data_ob$SQ1 <- as.factor(train_data_ob$SQ1)

summary(train_data_ob)
sum(is.na(train_data_ob))
train_data_ob <- na.omit(train_data_ob)
tree_ob1 <- ctree(point~SQ1+consistency+Q33 ,data=train_data_ob, )
plot(tree_ob1)


#total decision tree
summary(train_data_yb)
train_data_yb$SQ1 <- as.factor(train_data_yb$SQ1)
train_data_yb$GUMO <- as.factor(train_data_yb$GUMO)
train_data_yb$GENDER <- as.factor(train_data_yb$GENDER)
train_data_yb$INC <- as.factor(train_data_yb$INC)
train_data_yb$Q37 <- as.factor(train_data_yb$Q37)
train_data_yb$Q22 <- as.factor(train_data_yb$Q22)
train_data_yb$Q23 <- as.factor(train_data_yb$Q23)
train_data_yb$Q30 <- as.factor(train_data_yb$Q30)
train_data_yb$Q31 <- as.factor(train_data_yb$Q31)
train_data_yb$Q33 <- as.factor(train_data_yb$Q33)
tree_yb2 <- ctree(point~. ,data=train_data_yb)
plot(tree_yb2)

summary(train_data_mb)
train_data_yb$SQ1 <- as.factor(train_data_yb$SQ1)
train_data_mb$GUMO <- as.factor(train_data_mb$GUMO)
train_data_mb$GENDER <- as.factor(train_data_mb$GENDER)
train_data_yb$INC <- as.factor(train_data_yb$INC)
train_data_yb$Q37 <- as.factor(train_data_yb$Q37)
train_data_yb$Q22 <- as.factor(train_data_yb$Q22)
train_data_yb$Q23 <- as.factor(train_data_yb$Q23)
train_data_yb$Q30 <- as.factor(train_data_yb$Q30)
train_data_yb$Q31 <- as.factor(train_data_yb$Q31)
train_data_yb$Q33 <- as.factor(train_data_yb$Q33)
tree_mb2 <- ctree(point~. ,data=train_data_mb)
plot(tree_mb2)

summary(train_data_ob)
train_data_yb$SQ1 <- as.factor(train_data_yb$SQ1)
train_data_ob$GUMO <- as.factor(train_data_ob$GUMO)
train_data_ob$GENDER <- as.factor(train_data_ob$GENDER)
train_data_yb$INC <- as.factor(train_data_yb$INC)
train_data_yb$Q37 <- as.factor(train_data_yb$Q37)
train_data_yb$Q22 <- as.factor(train_data_yb$Q22)
train_data_yb$Q23 <- as.factor(train_data_yb$Q23)
train_data_yb$Q30 <- as.factor(train_data_yb$Q30)
train_data_yb$Q31 <- as.factor(train_data_yb$Q31)
train_data_yb$Q33 <- as.factor(train_data_yb$Q33)
tree_ob2 <- ctree(point~. ,data=train_data_ob)
plot(tree_ob2)

