# Reading statlog data
train_data <- read.csv(file.choose(), header = T)
test_data <- read.csv(file.choose(), header = T)

train_data$X <- NULL
test_data$X <- NULL

train_data1 <- train_data
test_data1 <- test_data
#table(test_data$OVERALL_DIAGNOSIS)

# Factor variables
train_data$OVERALL_DIAGNOSIS <- as.factor(train_data$OVERALL_DIAGNOSIS)
test_data$OVERALL_DIAGNOSIS <- as.factor(test_data$OVERALL_DIAGNOSIS)

# Partition data - train 70%, test 30%
set.seed(511)
#ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
#train <- data[ind ==1,]
#test <- data[ind ==2,]

## Logistic Regression
lrmodel <- glm(OVERALL_DIAGNOSIS ~., data = train_data, family = "binomial")
summary(lrmodel)

# Confustion matrix for training data
p3 <- predict(lrmodel, train_data, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train_data$OVERALL_DIAGNOSIS)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

varImp(lrmodel)

# Confustion matrix for test data
p4 <- predict(lrmodel, test_data, type = "response")
p4 <- ifelse(p4>0.5,1,0)
(tab4 <- table(predicted = p4, Actual = test_data$OVERALL_DIAGNOSIS))
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100


# Considering attributes which have significant value
lrmodel <- glm(OVERALL_DIAGNOSIS ~ + f4 + f13, data = train_data, family = "binomial")
summary(lrmodel)
# Confustion matrix for training data
p3 <- predict(lrmodel, train_data, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train_data$OVERALL_DIAGNOSIS)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Confustion matrix for test data

p4 <- predict(lrmodel, test_data, type = c("response"))
p4 <- ifelse(p4>0.5,1,0)
(tab4 <- table(predicted = p4, Actual = test_data$OVERALL_DIAGNOSIS))
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100

train_control <- trainControl(method="cv", number=25)
#install.packages("e1071")
#library("e1071")
model <- train(OVERALL_DIAGNOSIS ~., data = train_data,trControl=train_control, method="glm", family = "binomial")
model


library(ROCR)

##################### ROC curve for all variables ######################

prob <- predict(lrmodel, newdata=test_data1, type="response")
pred <- prediction(prob, test_data1$OVERALL_DIAGNOSIS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

############# ROC Curve for significant variables ###############3

prob1 <- predict(lrmodel, newdata=test_data1, type="response")
pred1 <- prediction(prob1, test_data1$OVERALL_DIAGNOSIS)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
plot(perf1, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1



########################################

#Roc curve
#install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(p4, test_data$OVERALL_DIAGNOSIS)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, text.adj = c(-0.2,1.7))

perf <- performance(ROCRpred, 'sens', 'spec')
perf
CUTOFF <- 0.96
ix <- which.min(abs(perf@alpha.values[[1]] - CUTOFF)) #good enough in our case
sensitivity <- perf@y.values[[1]][ix] #note the order of arguments to `perfomance` and of x and y in `perf`
specificity <- perf@x.values[[1]][ix]
sensitivity
specificity

#ROC CURVE
#install.packages("pROC")
library(pROC)

auc <- auc(test_data$OVERALL_DIAGNOSIS ~ p4)
print (auc)




