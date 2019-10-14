library(dplyr)    # This library is to convert into character
library(Amelia)   # To handle missing data
library(ggplot2)  # For Plotting
library(caTools)  # For splitting the data
library(ROCR)
library(class)
library(data.table)
library(reshape2)
library(randomForest)
library(party)    # FOr decision tree 
library(rpart)    # for Rpart 
library(rpart.plot) #for Rpart plot
library(lattice)  # Used for Data Visualization
require(caret)    # for data pre-processing
library(pROC) 
library(corrplot)  # for correlation plot
library(e1071)    # for ROC curve
library(RColorBrewer)

#Read data
Rain_Tomorrow = read.csv(file.choose())
View(Rain_Tomorrow)

summary(Rain_Tomorrow)
str(Rain_Tomorrow)

#Data Cleaning
#Extract month and year from the date
Rain_Tomorrow$Date = as.Date(Rain_Tomorrow$Date, "%d-%m-%Y")
Rain_Tomorrow$month = as.numeric(format(Rain_Tomorrow$Date,"%m"))
Rain_Tomorrow$Year = as.numeric(format(Rain_Tomorrow$Date,"%Y"))
View(Rain_Tomorrow)

#group the directions to reduce the number of directions
NE <- c('NNE','NE')
E <- c('ENE','E')
SE <- c('ESE','SE')
S <- c('SSE','S')
SW <- c('SSW','SW')
W <- c('WSW','W')
NW <- c('WNW','NW')
N <- c('NNW','N')
other <- c('NA')

group_Direction <- function(Dir){
  if(Dir %in% NE)
    return('NE')
  else
    if(Dir %in% E)
      return('E')
  else
    if(Dir %in% SE)
      return('SE')
  else
    if(Dir %in% S)
      return('S')
  else
    if(Dir %in% SW)
      return('SW')
  else
    if(Dir %in% W)
      return('W')
  else
    if(Dir %in% NW)
      return('NW')
  else
    if(Dir %in% N)
      return('N')
  else
    return('other')
}

Rain_Tomorrow$WindGustDir = sapply(Rain_Tomorrow$WindGustDir,group_Direction)
Rain_Tomorrow$WindDir9am = sapply(Rain_Tomorrow$WindDir9am, group_Direction)
Rain_Tomorrow$WindDir3pm = sapply(Rain_Tomorrow$WindDir3pm, group_Direction)
View(Rain_Tomorrow)

#convert categorical data into factors
Rain_Tomorrow$Location = as.numeric(factor(Rain_Tomorrow$Location))
Rain_Tomorrow$WindGustDir = factor(Rain_Tomorrow$WindGustDir, levels = c('N','NE','E','SE','S','SW','W','NW','other'), labels = c(1,2,3,4,5,6,7,8,0))
Rain_Tomorrow$WindDir9am = factor(Rain_Tomorrow$WindDir9am, levels = c('N','NE','E','SE','S','SW','W','NW','other'), labels = c(1,2,3,4,5,6,7,8,0))
Rain_Tomorrow$WindDir3pm = factor(Rain_Tomorrow$WindDir3pm, levels = c('N','NE','E','SE','S','SW','W','NW','other'), labels = c(1,2,3,4,5,6,7,8,0))
Rain_Tomorrow$RainToday = factor(Rain_Tomorrow$RainToday, levels = c('Yes','No','NA'), labels = c(1,0,2))
Rain_Tomorrow$RainTomorrow = factor(Rain_Tomorrow$RainTomorrow, levels = c('Yes','No'), labels = c(1,0))
View(Rain_Tomorrow)

Rain_Tomorrow$RainTomorrow <- as.factor(Rain_Tomorrow$RainTomorrow)

#Handling missing data
#Impute the missing data with mean
#install.packages("Hmisc")
library(Hmisc)
Rain_Tomorrow$MinTemp <- impute(Rain_Tomorrow$MinTemp,mean)
Rain_Tomorrow$MaxTemp <- impute(Rain_Tomorrow$MaxTemp,mean)
Rain_Tomorrow$Rainfall <- impute(Rain_Tomorrow$Rainfall, mean)
Rain_Tomorrow$Evaporation <- impute(Rain_Tomorrow$Evaporation,mean)
Rain_Tomorrow$Sunshine <- impute(Rain_Tomorrow$Sunshine,mean)
Rain_Tomorrow$WindGustSpeed <- impute(Rain_Tomorrow$WindGustSpeed, mean)
Rain_Tomorrow$WindSpeed9am <- impute(Rain_Tomorrow$WindSpeed9am, mean)
Rain_Tomorrow$WindSpeed3pm <- impute(Rain_Tomorrow$WindSpeed3pm, mean)
Rain_Tomorrow$Humidity9am <- impute(Rain_Tomorrow$Humidity9am, mean)
Rain_Tomorrow$Humidity3pm <- impute(Rain_Tomorrow$Humidity3pm, mean)
Rain_Tomorrow$Pressure9am <- impute(Rain_Tomorrow$Pressure9am, mean)
Rain_Tomorrow$Pressure3pm <- impute(Rain_Tomorrow$Pressure3pm, mean)
Rain_Tomorrow$Cloud9am <- impute(Rain_Tomorrow$Cloud9am, mean)
Rain_Tomorrow$Cloud3pm <- impute(Rain_Tomorrow$Cloud3pm, mean)
Rain_Tomorrow$Temp9am <- impute(Rain_Tomorrow$Temp9am, mean)
Rain_Tomorrow$Temp3pm <- impute(Rain_Tomorrow$Temp3pm, mean)

Rain_Tomorrow = Rain_Tomorrow[,-23]
Rain_Tomorrow = Rain_Tomorrow[,-1]

View(Rain_Tomorrow)

Rain_Tomorrow$RainToday = impute(Rain_Tomorrow$RainToday,2)
str(Rain_Tomorrow$RainToday)
levels(Rain_Tomorrow$RainToday)
#write.csv(Rain_Tomorrow, file = "FinalExported.csv",sep = ",")

#EDA 
boxplot(Rain_Tomorrow$month ~ Rain_Tomorrow$RainTomorrow, main = "Box plot for month")
boxplot(Rain_Tomorrow$Location ~ Rain_Tomorrow$RainTomorrow, main = "Box plot for Location")


mosaicplot(Rain_Tomorrow$WindGustDir ~ Rain_Tomorrow$RainTomorrow, color = 'skyblue')
mosaicplot(Rain_Tomorrow$WindDir9am ~ Rain_Tomorrow$RainTomorrow, color = 'skyblue')
mosaicplot(Rain_Tomorrow$WindDir3pm ~ Rain_Tomorrow$RainTomorrow, color = 'skyblue')
mosaicplot(Rain_Tomorrow$month ~ Rain_Tomorrow$RainTomorrow , color = 'skyblue')

ggplot(Rain_Tomorrow, aes(Location)) + geom_histogram(aes(fill = RainTomorrow), color = 'black', binwidth = 1) + theme_bw()

qqnorm(Rain_Tomorrow$Humidity9am)


#Split Data
set.seed(101)
sample <- sample.split(Rain_Tomorrow, SplitRatio = 0.7)
train <- subset(Rain_Tomorrow, sample == T)
test <- subset(Rain_Tomorrow, sample == F)

#Logistic Regression

train$RainTomorrow <- as.factor(train$RainTomorrow)
test$RainTomorrow <- as.factor(test$RainTomorrow)

logit_model <- glm(RainTomorrow ~ ., family = binomial(logit), data = train)
logit_model

logit_predictions <- predict(logit_model, newdata = test, type = 'response')
logit_predictions

joiner <- function(x){
  if (x >= 0.5){
    return(1)
  }else{
    return(0)
  }
}

logit_predictions <- sapply(logit_predictions, joiner)
logit_predictions

#Confusion Matrix
logit_predictions <- as.factor(logit_predictions)

confusionMatrix(logit_predictions, test$RainTomorrow)

# ROC Curve and Area under the curve


logit_predictTest <- data.frame("Probability" = predict(logit_model, test))

logit_RCRTest <- prediction(logit_predictTest$Probability, test$RainTomorrow)

logit_ROCRTestperformance <- performance(logit_RCRTest, "tpr", "fpr")

plot(logit_ROCRTestperformance,main="Logistic Regression ROC Curve")

logit_auc <- paste(c("Logistic Regression AUC ="),round(as.numeric(performance(logit_RCRTest,"auc")@y.values),digits=2),sep="")

legend("topleft",logit_auc, bty="n")

#Decision Tree Algorithm

#Check for the event rate
prop.table(table(train$RainTomorrow))
prop.table(table(test$RainTomorrow))

# DT using rpart algorithm
fit = rpart(RainTomorrow ~ ., data = train, method = "class", control = rpart.control(minsplit = 100,cp = 0.01))
rpart.plot(fit)

print(fit)
summary(fit)

prp(fit)
plotcp(fit)
printcp(fit)

#Checking COnfusion matrix on Test data
predtest <- predict(fit, test, type = "class" )
confusionMatrix(predtest, test$RainTomorrow)

# roc(testdata, prediction)
auctrain <- roc(as.numeric(train$RainTomorrow), as.numeric(predtr))
auctest <- roc(as.numeric(test$RainTomorrow), as.numeric(predtest))
print(auctrain)
print(auctest)

plot(auctrain, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctrain$auc[[1]],3)),col = 'blue')
# plot(auctrain,col = 'blue',main=paste('AUC:',round(auctrain$auc[[1]],3)))


plot(auctest, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctest$auc[[1]],3)),col = 'blue')
# plot(auctest,col = 'blue',main=paste('AUC:',round(auctest$auc[[1]],3)))
