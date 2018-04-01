# Reading cleveland data
data <- read.csv(file.choose(), header = T)
data$X <- NULL
#data$X.1 <- NULL
#data$X.2 <- NULL

# Factor variables
#data$num <- as.factor(data$num)

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

## Implenting tree model
#install.packages("tree")
library(tree)
tree_model <- tree(num ~ ., train)
plot(tree_model)
text(tree_model, pretty = 0)

## Predicting for test data
pred = predict(tree_model, test)
## calculating the error rate on test dataset
mean((pred-test$num)^2)
## plotting the prediction
plot(pred,test$num)


## Implementing Bagging Model
## mtry = 4 because 4 variables are important to reduce mean square error
#install.packages("randomForest")
library(randomForest)
fit = randomForest(num ~., train, mtry=4, importance = TRUE)
## Plotting the error reduced as number of tree increased. Total is 500.
plot(fit)

# use this model to do prediction on test data
pred = predict(fit, test)
plot(pred, test$num)
mean((pred-test$num)^2)

## Variable Importance Plot
varImpPlot(fit)
