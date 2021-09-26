#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#1) Identify the Problem Statement, What are you trying to solve?
#here the ask is:-
#"if the titanic ship is once again built, and people are boarded, and the
#journey starts again, then can you help us identify whether the person whom we
#are boarding will survive another crash or not?"

#2) Import the dataset
## Here, the target variable is "Survived" which is categorical in nature.

titanic <- read.csv(choose.files())

titanic

# Basic EDA

View(titanic)
head(titanic)
tail(titanic)
class(titanic)
names(titanic)

dim(titanic)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.
nrow(titanic)
ncol(titanic)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

# Structure check of the variables
str(titanic)

summary(titanic)

library(psych)  ### stats package - "psych" ####

describe(titanic)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc

#####passenger ID####
#everytime a new passenger comes its ID will be something different. 
#A person having certain ID has no relation to whether that person will 
#survive the crash or not
##if you include this column then ML algo might find some patterns/relations and give
#importance to ID also- that will be a useless model from the business perspective

##ALWAYS THINK FROM THE BUSINESS PERSPECTIVE
#The input I am choosing is that making sense? If no, then reject. 

#####Pclass#####
#People travelling in 1st class might have access to life boats/jackets - they have 
#higher chance of survival.

#####Name#####
#same concept like PassengerID
##you can't say whether a person will survive or not, based on his/her name.

#####Sex#####
#In the movie also, we saw who was over the plank. Here, gender is a very 
#important variable and we'll see in our data exploration also why it's important. 

#####Age#####
#It is directly related to the swimming capacity or harsh condition tolerance

#####ticket#####
##ticket no. has not relation will survival


#####Fare#####
##people paying higher fares might have better access to different facilities in the ship
#and that can increase their chances of survival

#####cabin#####
##in which cabin the people are travelling
##lot of missing values
summary(titanic)

##529 missing values out of 714 variables- so we don't choose that column


#####Embarked#####
##docking area where a passenger boards the ship
#if the person didn't board the ship before it crashed, then he/she will
#survive

###check if all the categorical variables are factor or not

factor_cols=c("Survived","Pclass")

for (cat_cols in factor_cols){
  titanic[ , cat_cols]=as.factor(titanic[ , cat_cols])
}

str(titanic)

# Whether it is a Regression problem or Classification?

#target variable - survive or not survive
##survival:Yes/1 or Survival:No/0

#3) Data Pre-processing
#To check missing values
## find the missing value by using visualization ##
install.packages("Amelia")
library(Amelia)

missmap(titanic, main="TITANIC - Finding Missing Data",
        col=c("red", "black"), legend = F)

#### to check where is missing data to see in charater or factor data ###
colSums(titanic=="") 
colSums(titanic=="")/nrow(titanic) #77%- Cabin, 0.2 % Embarked

colSums(is.na(titanic))
colSums(is.na(titanic))/nrow(titanic)#19.86% we will impute the missing value

boxplot(titanic$Age) 
mean(titanic$Age, na.rm = T)
titanic$Age[which(is.na(titanic$Age))] <- 29.69912
colSums(is.na(titanic))

#we will mode function to handle missing value in Embarked 
getmode <- function(titanic) {
  uniqv <- unique(titanic)
  uniqv[which.max(tabulate(match(titanic, uniqv)))]
}
getmode(titanic$Embarked)
#replace with mode 
titanic$Embarked[titanic$Embarked==""]<- getmode(titanic$Embarked)
colSums(titanic=="") #done with missing missing embarked categorise 

# Now there is no missing values

# Removing useless columns in the data, and explore the rest

titanic <- titanic[,-c(1,4,9,11)] 

# Checking for the presence of outliers in variables using boxplot.

boxplot(titanic$Age) 
quantile(titanic$Age, seq(0,1,0.02))
titanic$Age <- ifelse(titanic$Age>55,55, titanic$Age)
titanic$Age <- ifelse(titanic$Age<5,5, titanic$Age)
boxplot(titanic$Age)

#Outlier Treatment Done

# 4) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis ##################

# Multiple Continuous Variables
ColsForHist=c("Age","Fare")

#Splitting the plot window into four parts
par(mfrow=c(2,1))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(titanic[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}

# Multiple Categorical features
ColsForBar=c("Survived","Pclass","Embarked")

#Splitting the plot window into four parts
par(mfrow=c(2,2))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(titanic[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Spectral"))
}

################ Bivariate Analysis ###################

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Categorical vs Categorical -- Grouped Bar chart

# Categorical vs Continuous analysis-- Boxplot
#Here, "Age", "Fare"- continuous
#Survived - Categorical

par(mfrow=c(1,1))

boxplot(Age~Survived, data = titanic, col=brewer.pal(8,"Accent"))

boxplot(Fare~Survived, data = titanic, col=brewer.pal(8,"Accent"))

# Categorical vs Categorical analysis-- Grouped Bar chart

install.packages("ggplot2")
library(ggplot2) #### for creating graphics####

#Pclass vs Survived
ggplot(titanic, aes(fill=Survived, y=Survived, x=Pclass)) + 
  geom_bar(position="stack", stat="identity")

#Embarked vs Survived
ggplot(titanic, aes(fill=Survived, y=Survived, x=Embarked)) + 
  geom_bar(position="stack", stat="identity")


# Relationship between target variable and predictors variable
# Categorical vs Continuous --- ANOVA
# Categorical vs Categorical -- Chi-square test

################ ANOVA TEST ##############################
# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# H0 Null hypothesis : Variables are not correlated
# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

anova <- aov(Age~Survived, data = titanic)
anova
summary(anova)

anova <- aov(Fare~Survived, data = titanic)
anova
summary(anova)

################ CHI-SQUARE TEST ##############################
# Categorical vs Categorical
# H0 Null hypothesis : Variables are not correlated

# Small P-Value - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

# We do cross tabulation as the input and gives you the result

Chisqcols=c("Pclass","Sex","Embarked")

for(chi_cols in ColsForBar ){
  CrossTabResult=table(titanic[,c('Survived',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(ColsForBar)
  print(ChiResult)
}

#H0:the two columns are not correlated

#p is very low, 
#so we reject the null and conclude that these two columns are correlated

#5) Split the data into training and testing 

library(caTools)

set.seed(101)
split<- sample.split(titanic$Survived, SplitRatio = 0.80)
split

table(split)
# will show how many are true and false value

training <- subset(titanic, split==TRUE)
nrow(training)
test <- subset(titanic, split==FALSE)
nrow(test)

##########################################################################################
########################### LOGISTIC REGRESSION ##########################################
##########################################################################################

#6) Building logistic regression model glm 

classi <- glm(Survived~., data = training, family = 'binomial')
classi
summary(classi)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

classi1 <- glm(Survived~.-Parch-Fare-Embarked, data = training, family = 'binomial')
classi1
summary(classi1)

####################################################################################
# Null deviance: 949.9 -- system generated error without taking independent variable
# Residual deviance: 632.4 -- error with independent variable
# AIC: 644.4 -- adjested r-square in logistic regression, akike information criterian
#####################################################################################

#Lower the AIC value, better the model

#7) We can predict model by using test dataset

pred <- predict(classi1, newdata = test, type = "response")
pred

pred_thre_50 <- ifelse(pred>0.5,1,0)

# confusion matrix

cm <- table(test$Survived, pred_thre_50)
cm

########################################################
# Sensitivity / Recall = TP / (TP+FN)
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Specificity = TN / (TN+FP)
# Precision/PPV = TP / (TP+FP)
# False Negative value = FN/(FN+TN)
# False Positive value = FP / (FP+TP)
# F1-Measures = (2*Recall*Precision)/(Recall+Precision)
########################################################

# Accuracy = (TP+TN)/(TP+FP+FN+TN)
(92+47)/(92+18+21+47)

#accuracy = 78%

# Library for confusion matrix
library(caret)
library(e1071)

confusionMatrix(cm)
## Logistic Regression : Accuracy :78.09%  ##

# F1-Measures/F1-score = (2*Recall*Precision)/(Recall+Precision)

F1_measures <- (2*0.8142*0.8364)/(0.8142+0.8364)

#####################################
# Accuracy : 0.7809
# Sensitivity/Recall : 0.8142        
# Specificity : 0.7231          
# Pos Pred Value/Precision : 0.8364         
# Neg Pred Value : 0.6912          
# Balanced Accuracy : 0.7686
# F1-measures/F1-score : 0.8251507
#####################################

###################################################################################
############################## DECISION TREE ######################################
###################################################################################

library(rpart)

titanic$Survived <- as.numeric(titanic$Survived)
titanic$Survived <- ifelse(titanic$Survived==1,0,1)

dtree <- rpart(Survived ~ ., data=training, method = 'class')
summary(dtree)

# prediction of the model

pred_dtree <- as.numeric(predict(dtree, newdata = test, type = 'class'))
pred_dtree

pred_dtree <- ifelse(pred_dtree==1,0,1) 
pred_dtree

# confusion matrix

cm1 <- table(test$Survived, pred_dtree)
cm1

confusionMatrix(cm1)

## Decision Tree : Accuracy : 0.7978 ##

######################################################################################
################################ RANDOM FOREST #######################################
######################################################################################

library(randomForest)

# Building the model  
rf <- randomForest(Survived ~ ., data=training)

# prediction of the model

rf_pred <- predict(rf, newdata = test)

# confusion matrix

cm2 <- table(test$Survived, rf_pred)
cm2

confusionMatrix(cm2)

## Random Forest : Accuracy : 0.809 ##

###############################################################################################
############################## NAIVE BAYES THEOREM ############################################
###############################################################################################

library(e1071)
library(class)

#check dependent variable must be "Factor"
str(titanic)

nbt <- naiveBayes(Survived ~ ., data=training)
summary(nbt)

# prediction of the model

nbt_pred <- predict(nbt, newdata = test)
nbt_pred

# confusion matrix

cm3 <- table(test$Survived, nbt_pred)
cm3

confusionMatrix(cm3)

## Naive Bayes Theorem : Accuracy : 0.7397 ##

#########################################################################################################
################################# SUPPORT VECTOR MACHINE ################################################
#########################################################################################################

# kernel - linear/sigmoid/polynomial/rbf

# svm - kernel = linear

svm_linear <- svm(Survived ~ ., data=training, kernel='linear')

# prediction of the model

pred_svm_linear <- predict(svm_linear,newdata = test, type="class")
pred_svm_linear 

# confusion matrix

cm4 <- table(test$Survived, pred_svm_linear)
cm4

confusionMatrix(cm4)

## SVM - Kernel = "linear" : Accuracy : 0.7753 ##

###########################################################################################

# svm - kernel = Sigmoid

svm_sigmoid <- svm(Survived ~ ., data=training, kernel='sigmoid')

# prediction of the model

pred_svm_sigmoid <- predict(svm_sigmoid,newdata = test, type="class")
pred_svm_sigmoid 

# confusion matrix

cm5 <- table(test$Survived, pred_svm_sigmoid)
cm5
confusionMatrix(cm5)

## SVM - Kerel = "sigmoid" : Accuracy : 0.6629 

########################################################################################

# svm - kernel = polynomial

svm_polynomial <- svm(Survived ~ ., data=training, kernel='polynomial')

# prediction of the model

pred_svm_polynomial <- predict(svm_polynomial,newdata = test, type="class")
pred_svm_polynomial 

# confusion matrix

cm6 <- table(test$Survived, pred_svm_polynomial)
confusionMatrix(cm6)

## SVM - Kerel = "polynomial" : Accuracy : 0.7978 ##

##################################################################################################

# svm - kernel = radial

svm_rbf <- svm(Survived ~ ., data=training, kernel='radial')

pred_svm_rbf <- as.numeric(predict(svm_rbf,newdata = test, type="class"))
pred_svm_rbf <- ifelse(pred_svm_rbf==1,0,1)

cm7 <- table(test$Survived, pred_svm_rbf)
confusionMatrix(cm7)

## SVM - Kerel = "rbf" : Accuracy : 0.8146 ##

####################################################
# 1) Logistic Regression : Accuracy : 0.7809 ******
# 2) Decision Tree : Accuracy : 0.7978 *******
# 3) Random Forest : Accuracy : 0.809 ***************
# 4) Naive Bayes Theorem : Accuracy : 0.7697
# 5) SVM - Kerel = "linear" : Accuracy : 0.7753
# 6) SVM - Kerel = "sigmoid" : Accuracy : 0.6629
# 7) SVM - Kerel = "polynomial" : Accuracy : 0.7978
# 8) SVM - Kerel = "rbf" : Accuracy : 0.8146 *******
#####################################################

