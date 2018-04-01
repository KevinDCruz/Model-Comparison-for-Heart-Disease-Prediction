# Reading cleveland data
data <- read.csv(file.choose(), header = T)
data$X <- NULL
data$X.1 <- NULL
data$X.2 <- NULL

# Factor variables
data$num <- as.factor(data$num)

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

## Implenting tree model
library(tree)
tree_model <- tree(num ~ ., train)
#plot(tree_model)
#text(tree_model, pretty = 0)

# Confusion matrix - test
p2 <- predict(tree_model, test, type = 'class')
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$num))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2)) * 100
#Accuracy
(sum(diag(tab2))/sum(tab2)) * 100


## cross-validation to check where to stop pruning
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size,
     cv_tree$dev,
     type = 'b')
## Pruning the tree
pruned_model = prune.misclass(tree_model, best = 8)
plot(pruned_model)
text(pruned_model, pretty = 0)



# Prediction
# Confusion matrix - test
p2 <- predict(pruned_model, test, type = 'class')
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$num))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2)) * 100
#Accuracy
(sum(diag(tab2))/sum(tab2)) * 100
