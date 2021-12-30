# Wine project
# Penny Cookson
# 27/12/2021
# HarvardX Data Science Capstone
#########################################################
#Note: Code created and compiled in R version 3.6.3
# The code takes about 10 minutes to run


# SECTION 1 - START: # Load libraries and Download data
###################################################################################

#initialise libaries

#if the required libraries are not already installed then install them
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")


#load the libraries
library(tidyverse)
library(readr)
library(caret)
library(data.table)
library(randomForest)
library(gridExtra)

#Wine Quality Data Set
#P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
#Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.

#download the files to a local drive
url <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
download.file(url,"winequality-red.csv")

url <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality.names"
download.file(url,"winequality.names")

#load the local file
red <- read_csv2("winequality-red.csv")

#show any problems
problems()

# SECTION 1 - END # Load libraries and Download data
###################################################################################

# SECTION 2 - START: # Data quality checking, data cleaning 
###################################################################################

# correct the total sulfur dioxide for rows 1296 and 1297
# mapping was col_double() rather than col_number() so the values have been set to NA and need ot be corrected.
red[1296,"total sulfur dioxide"] <- 77.5
red[1297,"total sulfur dioxide"] <- 77.5

#Tidy up the column names
#The column names are set to use camel case
colnames(red) <- c("fixedAcidity","volatileAcidity","citricAcid","residualSugar","chlorides","freeSulfurDioxide","totalSulfurDioxide","density","pH","sulphates","alcohol","quality")

#convert non numeric attributes to a numeric data type
#some of them have been set to character by the import.
red<- red %>% mutate(volatileAcidity = as.numeric(volatileAcidity))
red<- red %>% mutate(citricAcid = as.numeric(citricAcid))
red<- red %>% mutate(chlorides = as.numeric(chlorides))
red<- red %>% mutate(density = as.numeric(density))
red<- red %>% mutate(sulphates = as.numeric(sulphates))

#check the structure of the data sets
str(red)

#check the number of rows
nrow(red)
#should be 1599

red <- red %>% mutate(quality = as.factor(quality))

#data quality checks
#check for null and is.na values, these should all return 0
nrow(red %>% filter(is.na(fixedAcidity))) 
nrow(red %>% filter(is.null(fixedAcidity)))
nrow(red %>% filter(is.na(volatileAcidity)))
nrow(red %>% filter(is.null(volatileAcidity)))
nrow(red %>% filter(is.na(citricAcid)))
nrow(red %>% filter(is.null(citricAcid)))
nrow(red %>% filter(is.na(residualSugar)))
nrow(red %>% filter(is.null(residualSugar)))
nrow(red %>% filter(is.na(chlorides)))
nrow(red %>% filter(is.null(chlorides)))
nrow(red %>% filter(is.na(freeSulfurDioxide)))
nrow(red %>% filter(is.null(freeSulfurDioxide)))
nrow(red %>% filter(is.na(totalSulfurDioxide)))
nrow(red %>% filter(is.null(totalSulfurDioxide)))
nrow(red %>% filter(is.na(density)))
nrow(red %>% filter(is.null(density)))
nrow(red %>% filter(is.na(pH)))
nrow(red %>% filter(is.null(pH)))
nrow(red %>% filter(is.na(sulphates)))
nrow(red %>% filter(is.null(sulphates)))
nrow(red %>% filter(is.na(alcohol)))
nrow(red %>% filter(is.null(alcohol)))
nrow(red %>% filter(is.na(quality)))
nrow(red %>% filter(is.null(quality)))


#find the number of red wines at each quality
red %>% group_by (quality) %>% summarize(Number = n()) 

#histogram of quality
red %>%
  ggplot(aes(quality,fill=quality)) +
  geom_bar() +
  xlab("Quality") +
  ggtitle("Distribution of Quality") 


# SECTION 2 - END: # Data quality checking, data cleaning 
###################################################################################


# SECTION 3 - START: # Investigate data graphically
###################################################################################

#alcohol
#box plot including the outliers
p <- red %>%
  ggplot(aes(alcohol, quality,fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Alcohol Content") +
  theme(legend.position = "none") +      
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
alcoholOutliers <- boxplot(red$alcohol , plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$alcohol %in% alcoholOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(alcohol, quality, fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Alcohol Content") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Alcohol Content")

#volatileAcidity
#box plot including the outliers
p <- red %>%
  ggplot(aes(volatileAcidity, quality,fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Volatile Acidity") +
  theme(legend.position = "none")+  
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
volatileAcidityOutliers <- boxplot(red$volatileAcidity, plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$volatileAcidity %in% volatileAcidityOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(volatileAcidity, quality, fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Volatile Acidity") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Volatile Acidity")


#sulphates
#box plot including the outliers
p <- red %>%
  ggplot(aes(sulphates, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Sulphates") +
  theme(legend.position = "none")+    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
sulphatesOutliers <- boxplot(red$sulphates , plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$sulphates %in% sulphatesOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(sulphates, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Sulphates") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Sulphates")


#totalSulfurDioxide
#box plot including the outliers
p <- red %>%
  ggplot(aes(totalSulfurDioxide, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Total Sulphur Dioxide") +
  theme(legend.position = "none")+    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
totalSulfurDioxideOutliers <- boxplot(red$totalSulfurDioxide , plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$totalSulfurDioxide %in% totalSulfurDioxideOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(totalSulfurDioxide, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Total Sulphur Dioxide") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Total Sulphur Dioxide")

#density
#box plot including the outliers
p <- red %>%
  ggplot(aes(density, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Density") +
  theme(legend.position = "none") +    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
densityOutliers <- boxplot(red$density , plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$density%in%  densityOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(density, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Density") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Density")

#chlorides
#box plot including the outliers
p <- red %>%
  ggplot(aes(chlorides, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Chlorides") +
  theme(legend.position = "none")+    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
chloridesOutliers <- boxplot(red$chlorides , plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$chlorides%in%  chloridesOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(chlorides, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Chlorides") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Chlorides")

#fixedAcidity
#box plot including the outliers
p <- red %>%
  ggplot(aes(fixedAcidity, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Fixed Acidity") +
  theme(legend.position = "none") +    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
fixedAcidityOutliers <- boxplot(red$fixedAcidity, plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$fixedAcidity%in%  fixedAcidityOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(fixedAcidity, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Fixed Acidity") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Fixed Acidity")

#pH
#box plot including the outliers
p <- red %>%
  ggplot(aes(pH, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("PH") +
  theme(legend.position = "none") +    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
pHOutliers <- boxplot(red$pH, plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$pH%in%  pHOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(pH, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("PH") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by PH")

#residualSugar
#box plot including the outliers
p <- red %>%
  ggplot(aes(residualSugar, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Residual Sugar") +
  theme(legend.position = "none") +    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
residualSugarOutliers <- boxplot(red$residualSugar, plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$residualSugar %in%  residualSugarOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(residualSugar, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Residual Sugar") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Residual Sugar")

#freeSulfurDioxide
#box plot including the outliers
p <- red %>%
  ggplot(aes(freeSulfurDioxide, quality,fill=quality)) +
  geom_boxplot() +
  ylab("Quality") + 
  xlab("Free Sulphur Dioxide") +
  theme(legend.position = "none") +    
  ggtitle("Original Data") 

#save the outliers from the boxplot function into a variable so they can be removed from the data set. 
freeSulfurDioxideOutliers <- boxplot(red$freeSulfurDioxide, plot=FALSE)$out
#remove the outliers from the data
rednoOut <- red[-which(red$freeSulfurDioxide %in%  freeSulfurDioxideOutliers),]

#box plot excluding the outliers
pno <- rednoOut %>%
  ggplot(aes(freeSulfurDioxide, quality, fill=quality)) +
  geom_boxplot() +
  ylab("") + 
  xlab("Free Sulphur Dioxide") +
  ggtitle("Excluding Outliers") 

#combine the 2 plots in a 2 X 1 grid
grid.arrange(p, pno, ncol = 2, nrow = 1, top = "Quality by Free Sulphur Dioxide")


# END 3 - START: # Investigate data graphically
###################################################################################



# SECTION 4 - START: # Split into train and test sets 
###################################################################################

# split the data into a test and train data set
# test set will be 20% of red wine data
set.seed(1)
test_index <- createDataPartition(y = red$quality, times = 1, p = 0.2, list = FALSE)
red_train<- red[-test_index,]
red_test <- red[test_index,]

nrow(red_train)
#1277
nrow(red_test)
#322

# train set is 80% of red wine data
#find the number of red wines at each quality in the train set.
red_train %>% group_by (quality) %>% summarise(number = n()) 

# test set is 20% of red wine data
#find the number of red wines at each quality in the test set.
red_test %>% group_by (quality) %>% summarise(number = n())

# SECTION 4 - END: # Split into train and test sets 
###################################################################################


# SECTION 5 - START: # models
###################################################################################

### model 1 ###################################
# train a random forest model using all attributes and the caret package
# try without tuning - leave it up to caret to self tune
#set a seed so the results are reproducable
set.seed(1)
#use the caret package train function and a random forest (rf) method
fitredrf <- train(quality ~ ., method = "rf", data = red_train)
#display the resulting tuning parameters
fitredrf$bestTune
#nodesize 1 mtry 2

# run the prediction on train set
predrrf <- predict(fitredrf, red_train)

# display the confusion matrix
cmrrf <- confusionMatrix(predrrf,red_train$quality)
cmrrf

#display the overall accuracy
cmrrf$overall["Accuracy"]
#0.7204969 

# then try tuning the nodesize and mtry parameters manually.
# tuning parameters - find mtry (although we expect caret to self tune that)
# assume a nodesize of 1 (the default for classification)
# 11 attributes so we try 1 to 11
# expecting value would be sqrt(num attributes)
# then try tuning the nodesize and mtry parameters manually.
# tuning parameters - find mtry (although we expect caret to self tune that)
# assume a nodesize of 1 (the default for classification)
# 11 attributes so we try 1 to 11
# expecting value would be sqrt(num attributes)
mtry <- seq(1,11,1)
#set a seed so the results are reproducable
set.seed(1)

#try values of mtry between 1 and 11 and identify the best accuracy
acc <- sapply(mtry, function(m){
  train(quality ~ ., method = "rf", data = red_train,
        tuneGrid = data.frame(mtry = m),
        nodesize = 1)$results$Accuracy
})

#plot the accuracy against the mtry values
qplot(mtry, acc)

#pick the value of mtry with the maximum accuracy
mtry = mtry[which.max(acc)]
mtry
#3

#tuning parameters - find nodesize
#  use the mtry found above
nodesize <- seq(1,15,1)
#set a seed so the results are reproducable
set.seed(1)

#try values of nodesize between 1 and 15 and identify the best accuracy
acc <- sapply(nodesize, function(n){
  train(quality ~ ., method = "rf", data = red_train,
        tuneGrid = data.frame(mtry = 3),
        nodesize = n)$results$Accuracy
})
#plot the accuracy against the nodesize values
qplot(nodesize, acc)

#pick the value of nodesize with the maximum accuracy
nodesize = nodesize[which.max(acc)]
nodesize
#3

# now use these tuning parameters
#set a seed so the results are reproducable
set.seed(1)
#use the train function of the caret package to train the random forest nodel using the tuning parameters identified
fitredrf <- train(quality ~ ., method = "rf", data = red_train,
                  tuneGrid = data.frame(mtry = 3),
                  nodesize = 3)

# run the prediction on train set
predrrf <- predict(fitredrf, red_train)
#save this for future
predrrftrn <- predrrf

# display the confusion matrix
cmrrf <- confusionMatrix(predrrf,red_train$quality)

#display the overall accuracy
cmrrf$overall["Accuracy"]

#0.7236025 


#### Consider whether we need all the variables ###############################################
#find variable importance
varImp(fitredrf)



### model 2 k-nearest neighbours ###################################
# remove outliers from train set 
#determine the outliers for each of the attributes below
#alcohol
alcoholOutliers <- boxplot(red_train$alcohol , plot=FALSE)$out

#totalSulfurDioxide
totalSulfurDioxideOutliers <- boxplot(red_train$totalSulfurDioxide , plot=FALSE)$out

#chlorides
chloridesOutliers <- boxplot(red_train$chlorides , plot=FALSE)$out

#fixedAcidity
fixedAcidityOutliers <- boxplot( red_train$fixedAcidity, plot=FALSE)$out

#pH
pHOutliers <- boxplot(red_train$pH, plot=FALSE)$out

#Create a new train set without the outliers
red_trainno <- red_train[-which(red_train$alcohol %in% alcoholOutliers
                                | red_train$totalSulfurDioxide %in% totalSulfurDioxideOutliers
                                | red_train$chlorides %in% chloridesOutliers
                                | red_train$fixedAcidity %in% fixedAcidityOutliers
                                | red_train$pH %in% pHOutliers),]

#remove residual Sugar and Free Sulphur Dioxide from the train set
red_trainno <- red_trainno %>% select(-freeSulfurDioxide,-residualSugar)

nrow(red_trainno)

#train the knn model
#set a seed so the results are reproducable
set.seed(1)
#use the train function of the caret package to train the knn model
#the data is nornalised first by the preProcess clause
fitredknn <- train(quality ~ ., method = "knn", preProcess = c("center","scale"), data = red_trainno)

# run the prediction on train set
predrknn <- predict(fitredknn, red_train)
#save this for future
predrknntrn <- predrrf

cmrknn <- confusionMatrix(predrknn,red_train$quality)
cmrknn

#display the overall accuracy
cmrknn$overall["Accuracy"]

#0.6061081 
#note that warning messages are displayed when red_trainno is used (the data set with outliers removed) rather than red_train
#it is assumed that this is because some samples do not have all the levels available


### model 3 Support Vector Model ##################################
#train the svm model
#set a seed so the results are reproducable
set.seed(1)
#use the train function of the caret package to train the svm model
#the data is nornalised first by the preProcess clause
fitredsvm <- train(quality ~ ., method = "svmRadial",preProcess = c("center","scale"),  data = red_trainno)

# run the prediction on train set
predrsvm <- predict(fitredsvm, red_train)
#save this for future
predrsvmtrn <- predrrf

cmrsvm <- confusionMatrix(predrsvm,red_train$quality)
cmrsvm

#display the overall accuracy
cmrsvm$overall["Accuracy"]
#0.5974941 


# SECTION 5 - END: # models
###################################################################################


# SECTION 6 - START: # Results
###################################################################################

#Model 1 Random Forest 
##########################
# run the random forest prediction on the test set
predrrf <- predict(fitredrf, red_test)

# display the confusion matrix
cmrrf <- confusionMatrix(predrrf,red_test$quality)
cmrrf

#display the overall accuracy
cmrrf$overall["Accuracy"]


#Model 2 KNN
##########################
# run the knn prediction on the test set
predrknn <- predict(fitredknn, red_test)

cmrknn <- confusionMatrix(predrknn,red_test$quality)
cmrknn

#display the overall accuracy
cmrknn$overall["Accuracy"]


#Model 3 SVM
##########################
# run the svm prediction on the test set
predrsvm <- predict(fitredsvm, red_test)

cmrsvm <- confusionMatrix(predrsvm,red_test$quality)
cmrsvm

#display the overall accuracy
cmrsvm$overall["Accuracy"]


# SECTION 6 - END: # Results
###################################################################################

# SECTION 7 - START: # combine models with and without outliers
###################################################################################

#this section tries combining the models using an approach with and without outliers to see if the result improves.  

#all the attributes are used
#all outliers are removed from one data set
#the prediction used depends on whether the values fall ibto the outliers.
#the apporach was to try and more effectively predict the loq and high quality values
#it did not improve the results so it has been discarded from the report.

#find the outliers for all the attributes
alcoholOutliers <- boxplot(red$alcohol , plot=FALSE)$out
volatileAcidityOutliers <- boxplot(red$volatileAcidity, plot=FALSE)$out
sulphatesOutliers <- boxplot(red$sulphates , plot=FALSE)$out
totalSulfurDioxideOutliers <- boxplot(red$totalSulfurDioxide , plot=FALSE)$out
densityOutliers <- boxplot(red$density , plot=FALSE)$out
chloridesOutliers <- boxplot(red$chlorides , plot=FALSE)$out
citricAcidOutliers <- boxplot(red$citricAcid , plot=FALSE)$out
fixedAcidityOutliers <- boxplot(red$fixedAcidity, plot=FALSE)$out
pHOutliers <- boxplot(red$pH, plot=FALSE)$out
residualSugarOutliers <- boxplot(red$residualSugar, plot=FALSE)$out
freeSulfurDioxideOutliers <- boxplot(red$freeSulfurDioxide, plot=FALSE)$out

#create a data set with no outliers
red_trainno <- red_train[-which(red_train$freeSulfurDioxide %in% freeSulfurDioxideOutliers
                                | red_train$alcohol %in% alcoholOutliers
                                | red_train$volatileAcidity %in% volatileAcidityOutliers
                                | red_train$sulphates %in% sulphatesOutliers
                                | red_train$totalSulfurDioxide %in% totalSulfurDioxideOutliers
                                | red_train$density %in% densityOutliers
                                | red_train$chlorides %in% chloridesOutliers
                                | red_train$citricAcid %in% citricAcidOutliers
                                | red_train$fixedAcidity %in% fixedAcidityOutliers
                                | red_train$pH %in% pHOutliers
                                | red_train$residualSugar %in% residualSugarOutliers),]


nrow(red_trainno)
#567

#create a data set with only outliers
red_trainout <- red_train[which(red_train$freeSulfurDioxide %in% freeSulfurDioxideOutliers
                                | red_train$alcohol %in% alcoholOutliers
                                | red_train$volatileAcidity %in% volatileAcidityOutliers
                                | red_train$sulphates %in% sulphatesOutliers
                                | red_train$totalSulfurDioxide %in% totalSulfurDioxideOutliers
                                | red_train$density %in% densityOutliers
                                | red_train$chlorides %in% chloridesOutliers
                                | red_train$citricAcid %in% citricAcidOutliers
                                | red_train$fixedAcidity %in% fixedAcidityOutliers
                                | red_train$pH %in% pHOutliers
                                | red_train$residualSugar %in% residualSugarOutliers),]

nrow(red_trainout)
#710

#find the number of red wines at each quality in the train and test sets
red_trainno %>% group_by (quality) %>% summarise(number = n()) 
red_trainout %>% group_by (quality) %>% summarise(number = n()) 


#add a column to indicate whether the data falls into the outliers
red_trainall <- red_train %>% mutate(outlier = (freeSulfurDioxide %in% freeSulfurDioxideOutliers
                                                | alcohol %in% alcoholOutliers
                                                | volatileAcidity %in% volatileAcidityOutliers
                                                | sulphates %in% sulphatesOutliers
                                                | totalSulfurDioxide %in% totalSulfurDioxideOutliers
                                                | density %in% densityOutliers
                                                | chlorides %in% chloridesOutliers
                                                | citricAcid %in% citricAcidOutliers
                                                | fixedAcidity %in% fixedAcidityOutliers
                                                | pH %in% pHOutliers
                                                | residualSugar %in% residualSugarOutliers))


# try without tuning - leave it up to caret to self tune
# data set without outliers
set.seed(1)
#train a model on only the data without outliers
fitredrfno <- train(quality ~ ., method = "rf", data = red_trainno)
fitredrfno$bestTune

predrrfno <- predict(fitredrfno, red_train)

cmrrfno <- confusionMatrix(predrrfno,red_train$quality)
cmrrfno

cmrrfno$overall["Accuracy"]
#0.7658575  


# try without tuning - leave it up to caret to self tune
# data set with only outliers
set.seed(1)
fitredrfout <- train(quality ~ ., method = "rf", data = red_trainout)
fitredrfno$bestTune

predrrfout <- predict(fitredrfout, red_train)

cmrrfout <- confusionMatrix(predrrfout,red_train$quality)
cmrrfout

cmrrfout$overall["Accuracy"]
#0.8324197 


#predict using all data
predrrf <- predict(fitredrf, red_train)
#predict using no outliers
predrrfno <- predict(fitredrfno, red_train)
#predict using only outliers
predrrfout <- predict(fitredrfno, red_train)

#record all the rredicitons in columns
red_trainall  <- red_trainall %>% mutate(predrrf = predrrf, predrrfno = predrrfno, predrrfout = predrrfout)

#set the prediction deoending on whether the data falls into the outliers
#3 combinations are tried
red_trainall <- red_trainall %>% mutate (pred1 = case_when(outlier==TRUE ~ predrrfout, TRUE ~ predrrf), pred2 = case_when(outlier==TRUE ~ predrrfout, TRUE ~ predrrfno), pred3 = case_when(outlier==FALSE ~ predrrfno, TRUE ~ predrrf))

#test the predictions against the real quality in the train set
cmr <- confusionMatrix(red_trainall$pred1,red_train$quality)
cmr$overall["Accuracy"]

cmr <- confusionMatrix(red_trainall$pred2,red_train$quality)
cmr$overall["Accuracy"]

cmr <- confusionMatrix(red_trainall$pred3,red_train$quality)
cmr$overall["Accuracy"]


#do the same with the test set
red_testall <- red_test %>% mutate(outlier = (freeSulfurDioxide %in% freeSulfurDioxideOutliers
                                | alcohol %in% alcoholOutliers
                                | volatileAcidity %in% volatileAcidityOutliers
                                | sulphates %in% sulphatesOutliers
                                | totalSulfurDioxide %in% totalSulfurDioxideOutliers
                                | density %in% densityOutliers
                                | chlorides %in% chloridesOutliers
                                | citricAcid %in% citricAcidOutliers
                                | fixedAcidity %in% fixedAcidityOutliers
                                | pH %in% pHOutliers
                                | residualSugar %in% residualSugarOutliers))
predrrf <- predict(fitredrf, red_test)
predrrfno <- predict(fitredrfno, red_test)
predrrfout <- predict(fitredrfno, red_test)

red_testall <- red_testall %>% mutate(predrrf = predrrf, predrrfno = predrrfno, predrrfout = predrrfout)

#set the prediction deoending on whether the data falls into the outliers
#3 combinations are tried
red_testall <- red_testall %>% mutate (pred1 = case_when(outlier==TRUE ~ predrrfout, TRUE ~ predrrf), pred2 = case_when(outlier==TRUE ~ predrrfout, TRUE ~ predrrfno),pred3 = case_when(outlier==FALSE ~ predrrfno, TRUE ~ predrrf))


cmr <- confusionMatrix(red_testall$pred1,red_testall$quality)
cmr$overall["Accuracy"]

cmr <- confusionMatrix(red_testall$pred2,red_testall$quality)
cmr$overall["Accuracy"]

cmr <- confusionMatrix(red_testall$pred3,red_testall$quality)
cmr$overall["Accuracy"]

#the original random forest
cmr <- confusionMatrix(red_testall$predrrf,red_testall$quality)
cmr$overall["Accuracy"]


#note that the accuracy is not improved over just using the ransom Forest on all data.

# SECTION 7 -END: # combine models
###################################################################################

# SECTION 8 - START: # combine models of different types
###################################################################################
#add all the model results to the train table
red_trainallmods <- red_train %>% mutate(predrrf = predrrftrn, predrknn = predrknntrn, predrrsvm = predrsvmtrn)
red_trainallmods <- red_trainallmods %>% mutate(pred = as.factor(apply(red_trainallmods[,13:15], 1, median)))

cmr <- confusionMatrix(red_trainallmods$pred,red_trainallmods$quality)
cmr$overall["Accuracy"]
                                                



#apply to test
#add all the model results to the train table
red_testallmods <- red_test %>% mutate(predrrf = predrrf, predrknn = predrknn, predrsvm = predrsvm)
red_testallmods <- red_testallmods %>% mutate(pred = as.factor(apply(red_testallmods[,13:15], 1, median)))

cmr <- confusionMatrix(red_testallmods$pred,red_testallmods$quality)
cmr$overall["Accuracy"]


#note that the accuracy is not improved over just using the ransom Forest on all data.

# SECTION 8 -END: # combine models of different types
###################################################################################

