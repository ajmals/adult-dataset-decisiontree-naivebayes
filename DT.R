

# IMPORT LIBRARIES --------------------------------------------------------

library(rpart)
library(rpart.plot)
library(party)



# DATA PARTITION ----------------------------------------------------------
adf_dtdata <- adf_no_missing
adf_dtdata$income <- factor(data3$income, levels=c("<=50K",">50K"), labels=c("0", "1"))

set.seed(1234)#the seed is needed so when the train and test data is constructed, it will give the same dataset
ind <- sample(2, nrow(adf_dtdata), replace=TRUE, prob=c(0.7, 0.3))
adf_DTtrainData <- adf_dtdata[ind==1,]
adf_DTvalidationData <- adf_dtdata[ind==2,]
table(adf_DTtrainData$income)



# DECISION TREE 1 MODELING ------------------------------------------------
tree1 = rpart(income ~ ., data=adf_DTtrainData, method="class")
print(tree1)


# DECISION TREE1 PLOTS ----------------------------------------------------
prp(tree1)
prp(tree1, type = 5)
rpart.plot(tree1, extra = 104, nn = TRUE)
plotcp(tree1)

# DECISION TREE1 PREDICTION TEST-----------------------------------------------
predict11 = predict(tree1, adf_DTvalidationData, type = "class")
confusionMatrix(predict11, adf_DTvalidationData$income)

# DECISION TREE1 PREDICTION TRAIN-----------------------------------------------
predict12 = predict(tree1, adf_DTtrainData, type = "class")
confusionMatrix(predict12, adf_DTtrainData$income)



# DECISION TREE 2 MODELING ------------------------------------------------

#Here we use tree with parameter settings.
tree2 = rpart(income ~ ., data=adf_DTtrainData, method="class", minsplit = 1, minbucket = 10, cp = -1)

# DECISION TREE2 PLOTS ----------------------------------------------------
prp (tree2)
print(tree2)
summary(tree2)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)


# DECISION TREE2 PREDICTION WITH TEST SET -----------------------------------------------
# Now we predict and evaluate the performance of the trained tree model 
predict21 = predict(tree2, adf_DTtrainData, type="class")
# Now examine the values of Predict. These are the class probabilities
head(predict21)
#Confusion matrix for tree 2
confusionMatrix(predict21, adf_DTtrainData$income)




# DECISION TREE2 PREDICTION WITH TEST SET -----------------------------------------------
# Now we predict and evaluate the performance of the trained tree model 
predict2 = predict(tree2, adf_DTvalidationData, type="class")
# Now examine the values of Predict. These are the class probabilities
head(predict2)
#Confusion matrix for tree 2
confusionMatrix(predict2, adf_DTvalidationData$income)




"""
# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# By default threshold is set at 0.5 to produce the classes
"""

Predict2 = predict(tree2, testData, type = "class")
Predict


# Producing confusion matrix
confusionMatrix(predict, testData$income)
Confusion_matrix = table(Predict, testData$income)
print(Confusion_matrix)


# ROC curve
install.packages("ROCR")
library(ROCR)
# install.packages("gplots")

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

PredictROC = predict(tree2, testData)
PredictROC
PredictROC[,2]

pred = prediction(PredictROC[,2], validationData$Class)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve

auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc