# Reading cleveland data
data <- read.csv(file.choose(), header = T)
data$X <- NULL
data$X.1 <- NULL
data$X.2 <- NULL

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]


## Implementing gradient boosting model
#install.packages("gbm")
library(gbm)
require(gbm)
cleveland.boost=gbm(num ~ . ,train,
                 distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
## Showing variable importance / Relative Influence
summary(cleveland.boost)

#Plot of Response variable with chol variable
plot(cleveland.boost,i="chol") 

#Plot of Response variable with chol variable
plot(cleveland.boost,i="thal")

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(cleveland.boost,train,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(train,apply( (predmatrix-num)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

