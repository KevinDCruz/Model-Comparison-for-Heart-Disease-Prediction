library(ROCR)

##################### ROC curve for all variables ######################

prob <- predict(lrmodel, newdata=test, type="response")
pred <- prediction(prob, test$num)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

############# ROC Curve for significant variables ###############3

prob1 <- predict(lrmodel, newdata=test, type="response")
pred1 <- prediction(prob1, test$num)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
plot(perf1, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1