# MLP using Keras for classification
#install.packages("keras")
library(keras)
#install_keras()

#getwd()

# Read data. Reads from a file.
#data <- read.csv(file.choose(), header = T)
#str(data)

# Change to matrix
nn_data <- as.matrix(adf_no_missing)
dimnames(nn_data) <- NULL
dimnames(nn_data)


head(nn_data[, 1])

str(nn_data)
# Normalize using keras package
nn_data[, 1:14] <- normalize(nn_data[,1:14])
# convert the taraget variable from 1,2,3 to 0,1,2
nn_data[,15] <- as.numeric(nn_data[,15]) -1
summary(nn_data)
head(nn_data)

head(nn_data[,35])

# Data partition
set.seed(1234)
ind <- sample(2, nrow(nn_data), replace = T, prob = c(0.7, 0.3))
training <- nn_data[ind == 1, 1:21]
test <- nn_data[ind == 2, 1:21]
trainingtarget <- nn_data[ind==1, 22]
testtarget <- nn_data[ind==2, 22]

# One Hot Encoding using keras package
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
print(testLabels)

# Create sequential model
model <- keras_model_sequential()
# pipe function %>%
model %>%
  layer_dense(units=8, activation = 'relu', input_shape = c(21)) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model)

# Compile
# for binary class use binary_crossentropy
model %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit model
# default batch_size is 32
history <- model %>%
  fit(training,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history)

# Evaluate model with test data
model1 <- model %>%
  evaluate(test, testLabels)

# Prediction & confusion matrix - test data
prob <- model %>%
  predict_proba(test)

pred <- model %>%
  predict_classes(test)
table(Predicted = pred, Actual = testtarget)
table1 <- table(Predicted = pred, Actual = testtarget)
table1

# column bind
cbind(prob, pred, testtarget)

# Fine-tune model
"""
# Repeat 1, build model 2
model %>%
layer_dense(units=50, activation = 'relu', input_shape = c(21)) %>%
layer_dense(units = 3, activation = 'softmax')
summary(model)

# Repeat 2, build model 3
model %>%
layer_dense(units=50, activation = 'relu', input_shape = c(21)) %>%
layer_dense(units=8, activation = 'relu') %>%
layer_dense(units = 3, activation = 'softmax')
summary(model)
Repeat 2

"""
