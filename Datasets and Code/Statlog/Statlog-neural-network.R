# Reading cleveland data
data <- read.csv(file.choose(), header = T)
data$X <- NULL
data$X.1 <- NULL
data$X.2 <- NULL

## Neural Network
#Normalize
data <- (data - min(data, na.rm=TRUE))/(max(data,na.rm=TRUE) - 
                                             min(data, na.rm=TRUE))
# Partition
set.seed(511)
ind <- sample(2, nrow(data), replace =T, prob = c(0.7,0.3))
train <-data[ind==1,]
test <- data[ind ==2,]

# Neural network model
library(neuralnet)
n <- names(train)
f <- as.formula(paste("num ~", paste(n[!n %in% "num"], collapse = " + ")))
#nmodel <- neuralnet(f, data = train, hidden = 4, stepmax=1e6)
nmodel <- neuralnet(f, data=train, hidden=2, err.fct = "ce",
                    linear.output = FALSE)
#nmodel
plot(nmodel)
p <- nmodel$net.result[[1]]

## Confidence Interval
ci = confidence.interval(nmodel, alpha = 0.5)
ci

## Visualizing the results
par(mfrow=c(2,2))
gwplot(nmodel, selected.covariate = "thal",
       min = 2.5, max = 5)
gwplot(nmodel, selected.covariate = "sex",
       min = 2.5, max = 5)
gwplot(nmodel, selected.covariate = "cp",
       min = 2.5, max = 5)
gwplot(nmodel, selected.covariate = "trestbps",
       min = 2.5, max = 5)



# Prediction train
pred <- compute(nmodel,train[,-1])
#  Confusion matrix - train data
p5 <- pred$net.result

p5 <- ifelse(p5>0.5,1,0)
tab5 <- table(predicted=p5, Actual = train$num)

# Miss clacification Error - train data
(1- sum(diag(tab5)/sum(tab5))) * 100
# Accuracy - train data
(sum(diag(tab5)/sum(tab5))) * 100

# Prediction test
pred <- compute(nmodel,test[,-1])
# Confusion matrix - test data
p6 <- pred$net.result
p6 <- ifelse(p6>0.5,1,0)
tab6 <- table(predicted=p6, Actual = test$num)
# Miss clacification Error - train data
(1- sum(diag(tab6)/sum(tab6))) * 100
# Accuracy - train data
(sum(diag(tab6)/sum(tab6))) * 100



###### Using Backpropagation Algorithm and Playing with
###### Learning Rate and entropy
nn.bp <- neuralnet(f, data=train, hidden=2, learningrate = 0.01,
                   algorithm = "backprop", err.fct = "ce",
                    linear.output = FALSE)

plot(nn.bp)
