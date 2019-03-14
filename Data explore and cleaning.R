
# Importing libraries -----------------------------------------------------
library(DataExplorer)
library(caret)
library(e1071)

#install.packages("keras")
library(keras)
#install_keras()

levels_factors <- function(mydata) {
  col_names <- names(mydata)
  for (i in 1:length(col_names)) {
    if (is.factor(mydata[, col_names[i]])) {
      message(noquote(paste("Covariate ", "*", 
                            col_names[i], "*", 
                            " with factor levels:", 
                            sep = "")))
      print(levels(mydata[, col_names[i]]))
    }
  }
}
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}



# IMPORT DATA -------------------------------------------------------------
setwd("C:/Users/User/Google Drive/APU/AML/assignment2/adult_dataset")
getwd()
fileName <- 'adult.data.csv'

adf<- read.table(file = fileName, header = FALSE, sep = ",", na.strings = "?", 
                 strip.white = TRUE, stringsAsFactors = TRUE, 
                 col.names=c("age","workclass","fnlwgt","education",
                             "education_num","marital_status","occupation",
                             "relationship","race","sex","capital_gain",
                             "capital_loss","hours_per_week","native_country",
                             "income"))

# DATA EXPLORATION  -------------------------------------------------------

str (adf)
plot_str(adf)
plot_missing(adf)
sum(colSums(is.na(adf)))

levels_factors (adf)
propmiss(adf)
plot_histogram(adf)
plot_missing(adf)

plot_bar(adf$workclass)
plot_bar(adf$workclass)

plot_correlation(adf,'discrete')

plot(table(adultdata1$native_country))



# Check categorical variable ----------------------------------------------
# check categorical variables variance with bar charts and continous with histograms

plot_histogram(adf$age)
plot_bar(adf$workclass)
plot_histogram(adf$education_num)
plot_bar(adf$education)
plot_bar(adf$marital_status)
plot_bar(adf$occupation)
plot_bar(adf$relationship)
plot_bar(adf$race)
plot_bar(adf$sex)
plot_histogram(adf$capital_gain)
plot_histogram(adf$capital_loss)
plot_bar(adf$native_country)
plot_bar(adf$income)

#additional data explorer report
create_report(adf)


# calculating missing value percentage
sum(is.na(adf))
sum(is.na(adf)/32561) * 100
plot_missing(adf)


# DATA REDUCTION -----------------------------------------------------------

#Native country south converted to Missing values (71 observations removed)
adf[adf == "South"] <- NA

#Grouping categorical varaibles
levels(adf$workclass)<-
  list(goverment=c("Federal-gov","Local-gov", "State-gov"),
       private=c("Private", "Self-emp-inc", "Self-emp-not-inc"),
       unemployed=c("Without-pay","Never-worked"))

levels(adf$education) <- 
  list(dropout=c("10th","11th", "12th","1st-4th", "5th-6th", "7th-8th","9th", "Preschool"),
       #associates=c("Assoc-acdm","Assoc-voc",""),
       undergraduate=c("Bachelors","Assoc-acdm","Assoc-voc"),
       #doctorate=c("Doctrate"),
       high_school_graduate=c("HS-grad", "Some-college"),
       graduate_and_profSchool=c("Masters","Prof-school", "Doctorate" ))
# professional_school=c("Prof-school"))

levels(adf$marital_status) <- 
  list(divorced=c("Divorced","Separated", "Widowed"),
       married=c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent"),
       single=c("Never-married"))

levels(adf$occupation)<- 
  list(clerical=c("Adm-clerical"), 
       low_skill_labr=c("Craft-repair",
                        "Handlers-cleaners",
                        "Machine-op-inspct",
                        "Other-service",
                        "Priv-house-serv",
                        "Prof-specialty",
                        "Protective-serv"),
       high_skill_labr=c("Sales","Tech-support","Transport-moving","Armed-Forces", "Exec-managerial"),
       agricultr_fishin=c("Farming-fishing"))


levels(adf$relationship)<- 
  list(spouse=c("Husband","Wife"), 
       #outofamily=c("Not-in-family"),
       single=c("Unmarried", "Not-in-family"), 
       relative=c("Other-relative"), 
       ownchild=c("Own-child"))

levels(adf$race) <-
  list(white=c("White"),
       black=c("Black"),
       other=c("Asian-Pac-Islander","Amer-Indian-Eskimo","Other"))

levels(adf$native_country)<- 
  list(#South=c("South"),# south is a not a country name
    Asia=c("China","India","Hong","Iran",
           "Philippines","Taiwan", "Vietnam",
           "Laos","Cambodia","Thailand","Japan"),
    nonUS_America=c("Canada","Cuba","Dominican-Republic",
                    "Guatemala","Haiti","Honduras",
                    "Jamaica","Mexico","Nicaragua",
                    "Puerto-Rico","El-Salvador",
                    "Ecuador","Peru","Columbia",
                    "Trinadad&Tobago"), 
    Europe=c("France","Germany","Greece","Holand-Netherlands",
             "Italy","Hungary","Ireland","Poland","Portugal",
             "Scotland","England","Yugoslavia"),
    United_States=c("Outlying-US(Guam-USVI-etc)","United-States"))




# Label encoding ----------------------------------------------------------
adf_en <- adf
adf_en$workclass = factor(adf_en$workclass,
                      levels = c('goverment', 'private', 'unemployed'),
                      labels = c(1, 2, 3))
adf_en$education = factor(adf_en$education,
                       levels = c('dropout', 'undergraduate', 'high_school_graduate', 'graduate_and_profSchool'),
                       labels = c(1, 2, 3, 4))
adf_en$marital_status = factor(adf_en$marital_status,
                       levels = c('divorced', 'married', 'single'),
                       labels = c(1, 2, 3))
adf_en$occupation = factor(adf_en$occupation,
                        levels = c('clerical', 'low_skill_labr', 'high_skill_labr', 'agricultr_fishin' ),
                        labels = c(1, 2, 3, 4))
adf_en$relationship = factor(adf_en$relationship,
                        levels = c('spouse', 'single', 'relative', 'ownchild' ),
                        labels = c(1, 2, 3, 4))
adf_en$race = factor(adf_en$race,
                  levels = c('white', 'black', 'other'),
                  labels = c(1, 2, 3))

adf_en$sex = factor(adf_en$sex,
                  levels = c('Female', 'Male'),
                  labels = c(1, 2))

adf_en$native_country = factor(adf_en$native_country,
                            levels = c('Asia', 'nonUS_America', 'Europe', 'United_States'),
                            labels = c(1, 2, 3, 4))

adf_en$income = factor(adf_en$income,
                    levels = c('<=50K', '>50K'),
                    labels = c(1, 2))

str(adf_en)






# DATA CLEANING: -------------------------------------------------

#plot missing values
plot_missing(adf)
str(adf)
table (adf$native_country)

#removing missing values
adf_no_missing = na.omit(adf_en)
rownames(adf_no_missing) <- 1:nrow(adf_no_missing) # recorrecting row numbers



#plot missing values to confirm that it has been removed or imputed
plot_missing(adf_no_missing)
table (adf_no_missing$native_country)



# Missig value treatment: impute by mean {Hmisc}


# check categorical variables variance with bar charts
plot_bar(adf_no_missing$workclass)
plot_bar(adf_no_missing$education)
plot_bar(adf_no_missing$marital_status)
plot_bar(adf_no_missing$occupation)
plot_bar(adf_no_missing$relationship)
plot_bar(adf_no_missing$race)
plot_bar(adf_no_missing$sex)
plot_bar(adf_no_missing$native_country)

levels_factors (adf_no_missing)
plot_missing(adf_no_missing)

prop.table(table(adf_no_missing$native_country))

plot_missing(adf)
plot_correlation(adf_no_missing) # marital_status married and relationship spouse are highly correlated.


#sum(is.na(adf_remove_missing)/30162) * 100
#plot(table(adf_remove_missing$workclass,adf_remove_missing$marital_status))

# NORMALIZING -------------------------------------------------------------
# Normalize using keras package
#adf_no_missing[1, 14] <- normalize(adf_no_missing[,1:14])
#adf_no_missing1 <- normalize(as.integer(adf_no_missing[,1:14]))


adf_normalized <- adf_no_missing
adf_normalized$age <- (adf_no_missing$age - min(adf_no_missing$age))/(max(adf_no_missing$age) - min(adf_no_missing$age))
adf_normalized$fnlwgt <- (adf_no_missing$fnlwgt - min(adf_no_missing$fnlwgt))/(max(adf_no_missing$fnlwgt) - min(adf_no_missing$fnlwgt))
adf_normalized$education_num <- (adf_no_missing$education_num - min(adf_no_missing$education_num))/(max(adf_no_missing$education_num)-min(adf_no_missing$education_num))
adf_normalized$capital_gain <- (adf_no_missing$capital_gain - min(adf_no_missing$capital_gain))/(max(adf_no_missing$capital_gain)-min(adf_no_missing$capital_gain))
adf_normalized$capital_loss <- (adf_no_missing$capital_loss - min(adf_no_missing$capital_loss))/(max(adf_no_missing$capital_loss)-min(adf_no_missing$capital_loss))
adf_normalized$hours_per_week <- (adf_no_missing$hours_per_week - min(adf_no_missing$hours_per_week))/(max(adf_no_missing$hours_per_week)-min(adf_no_missing$hours_per_week))


summary(adf_normalized)
head(adf_normalized)

# convert the taraget variable from 1,2,3 to 0,1,2
#adf_en[,15] <- as.numeric(adf_en[,15]) -1


# DATA PARTITIONING ---------------------------------------------------------------

# stratified sampling with 70% for training and 30% for test 
train.index <- createDataPartition(adf_normalized$income, p = .7, list = FALSE)
trainAdf <- adf_normalized[ train.index,]
validationAdf <- adf_normalized[-train.index,]

#check proportion of distribution for income after splitting
prop.table(table(trainAdf$income))
prop.table(table(validationAdf$income))



# HOT ENCODING WITH DATA EXPLORER------------------------------------------------------------

#dummifing all the categoricals (data explorer package)
adf_DExpdummyadf = dummify(trainAdf)
str(dummyadf)
head(dummyadf)
adf_DExpdummyadf <- adf_DExpdummyadf[,-35]



# HOT ENCODING WITH CARET -------------------------------------------------

#dummifing all the categoricals (caret) | gives a matrix output <test>
head(model.matrix( ~., data=trainAdf))
caretDummyModel = dummyVars( ~., data = trainAdf)
head(predict(caretDummyModel, newdata = trainAdf))
adf_caretDummy_train = predict(caretDummyModel, newdata = trainAdf)

#dummifing all the categoricals (caret) | gives a matrix output <validation>
head(model.matrix( ~., data=validationAdf))
caretDummyModel = dummyVars( ~., data = validationAdf)
head(predict(caretDummyModel, newdata = validationAdf))
adf_caretDummy_validation = predict(caretDummyModel, newdata = validationAdf)


adf_DExpdummyadf <- adf_DExpdummyadf[,-35]

#adf_en2 <- names(adf_en) [!names(adf_en) %in% c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]

#validationAdf[,!(names(validationAdf) %in% "income")]


str(caretDummy)
head(adf_no_missing$age)

