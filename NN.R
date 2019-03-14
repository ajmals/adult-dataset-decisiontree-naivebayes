# Neural Networks
library(neuralnet)

nn_adf_train <- adf_caretDummy_train



nn_adf_train <- nn_adf_train[,-35]

nn_adf_train$income <- as.integer(as.character(nn_adf_train$income)) 


nnadftrain <- adf_DExpdummyadf[,-35]

nnadftrain$income_1 < - as.numeric(as.character(nnadftrain$income_1))

n <-names(nn_adf_train)

set.seed(1234)
# ce - cross entropy
n <- neuralnet(income_1~age +
                 fnlwgt +
                 education_num +
                 capital_gain +
                 capital_loss +
                 hours_per_week +
                 workclass_1 +
                 workclass_2 +
                 workclass_3 +
                 education_1 +
                 education_2 +
                 education_3 +
                 education_4 +
                 marital_status_1 +
                 marital_status_2 +
                 marital_status_3 +
                 occupation_1 +
                 occupation_2 +
                 occupation_3 +
                 occupation_4 +
                 relationship_1 +
                 relationship_2 +
                 relationship_3 +
                 relationship_4 +
                 race_1 +
                 race_2 +
                 race_3 +
                 sex_1 +
                 sex_2 +
                 native_country_1 +
                 native_country_2 +
                 native_country_3 +
                 native_country_4
                 ,
               #n <- neuralnet(admit~.,
               data = nnadftrain,
               hidden = 5,
               err.fct = "SSE",
               linear.output = FALSE)
plot(n)

str(nnadftrain)
