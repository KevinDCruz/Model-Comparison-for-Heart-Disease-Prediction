# Reading statlog data
data <- read.csv(file.choose(), header = T)
data$X <- NULL
data1 <- data

# Factor variables
data$num <- as.factor(data$num)

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

#install.packages("caret")
library(caret)
## Logistic Regression
lrmodel <- glm(num ~., data = train, family = "binomial")
summary(lrmodel)

# Confustion matrix for training data
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train$num)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Confustion matrix for test data
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
tab4 <- table(predicted = p4, Actual = test$num)
tab4
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100


# Considering attributes which have significant value
lrmodel <- glm(num ~ + sex + cp + trestbps + chol + thalach + exang + ca + thal, data = train, family = "binomial")
summary(lrmodel)


# Confustion matrix for training data
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train$num)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Confustion matrix for test data
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
tab4 <- table(predicted = p4, Actual = test$num)
tab4
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100

############ Cross Validation #########################
train_control <- trainControl(method="cv", number=25)
#install.packages("e1071")
#library("e1071")
model <- train(num ~., data = data,trControl=train_control, method="glm", family = "binomial")
model

