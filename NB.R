library(DataExplorer)
#library(naniar)
library(caret)
#library(ggplot2)
#library(dplyr)
library(e1071)


#training Naive Bayes classifier {e1071}
NBclassifier_e1071 = naiveBayes(income ~ ., data = trainAdf)
summary(NBclassifier_e1071)

#naiveBayes(formula, data, laplace = 0, ..., subset, na.action = na.pass)



# Predicting using Naive Bayes model{caret} and accuracy results from confusion matrix{caret} (train dataset)
y_pred_train1 = predict(NBclassifier_e1071, newdata = trainAdf[,!(names(trainAdf) %in% "income")])
confusionMatrix(data = y_pred_train1, trainAdf$income)


# Predicting using Naive Bayes model{caret} and accuracy results from confusion matrix{caret} (validation dataset)
y_pred_validation1 = predict(NBclassifier_e1071, newdata = validationAdf[,!(names(validationAdf) %in% "income")])
confusionMatrix(data = y_pred_validation1, validationAdf$income)



#training Naive Bayes model using {caret} package with 10 fold cross validation
#NBclassifierCaretCV = train(x= trainAdf[,-10],y=trainAdf$income, 'nb', trControl = trainControl(method ='cv', number = 10))
NBclassifierCaretCV = train(income ~ ., data = trainAdf, 'nb', trControl = trainControl(method ='cv', number = 10))

CVtrainDataset = predict (NBclassifierCaretCV, newdata = trainAdf[,!(names(trainAdf) %in% "income")])
# Confusion matrix and a summary / using caret package
confusionMatrix(data = CVtrainDataset, trainAdf$income)
