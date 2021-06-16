# IMPORTING THE DATA:

train_file = "adult.data"; test_file = "adult.test"

if (!file.exists (train_file))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                 destfile = train_file)

if (!file.exists (test_file))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",
                 destfile = test_file)


#Assigning column names
colNames = c ("age", "workclass", "fnlwgt", "education",
              "educationnum", "maritalstatus", "occupation",
              "relationship", "race", "sex", "capitalgain",
              "capitalloss", "hoursperweek", "nativecountry",
              "incomelevel")

#Reading training data
train_set = read.table (train_file, header = FALSE, sep = ",",
                        strip.white = TRUE, col.names = colNames,
                        na.strings = "?", stringsAsFactors = TRUE)
#Reading testing data
test_set = read.table (test_file, header = FALSE, sep = ",",
                       strip.white = TRUE, col.names = colNames,
                       na.strings = "?", fill = TRUE, stringsAsFactors = TRUE)
test_set$age = as.integer(as.character(test_set$age))
test_set = na.omit(test_set) 

#Removing NAs
train_set = train_set[!is.na (train_set$workclass) & !is.na (train_set$occupation), ]
train_set = train_set[!is.na (train_set$nativecountry), ]

test_set= test_set[!is.na (test_set$workclass) & !is.na (test_set$occupation), ]
test_set= test_set[!is.na (test_set$nativecountry), ]

#Removing unnecessary variables

train_set$fnlwgt = NULL
test_set$fnlwgt = NULL


#Loading Necessary Library:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

#Building the model: Boosting

set.seed (32323, sample.kind = "Rounding")

trCtrl = trainControl(method = "cv", number = 10)

boostFit = train(incomelevel ~ age + workclass + education + educationnum +
                   maritalstatus + occupation + relationship +
                   race + capitalgain + capitalloss + hoursperweek +
                   nativecountry, trControl = trCtrl,
                 method = "gbm", data = train_set, verbose = FALSE)
#checking the accuracy
confusionMatrix(train_set$incomelevel, predict (boostFit, train_set))

#Building the model: Random Forest

#Building the model: Random Forest

set.seed(14, sample.kind = "Rounding")
train_rf <- train(incomelevel ~ age + workclass + education + educationnum +
                    maritalstatus + occupation + relationship +
                    race + capitalgain + capitalloss + hoursperweek +
                    nativecountry, method = "rf", ntree = 10,
                    tuneGrid = data.frame(mtry = seq(1:5)), 
                    data = train_set)

#checking the accuracy

confusionMatrix (train_set$incomelevel, predict(train_rf, train_set))

#Testing model: Since with Boosting Algorithm we got the highest accuracy on Train set 
test_set$predicted = predict(boostFit, test_set)
table(test_set$incomelevel, test_set$predicted)

actuals_preds <- data.frame(cbind(actuals=test_set$incomelevel, predicted=test_set$predicted)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)


# Defining RMSE:
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

RMSE(actuals_preds$actuals, actuals_preds$predicted)

























































