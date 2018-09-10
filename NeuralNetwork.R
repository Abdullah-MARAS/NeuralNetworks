#https://www.youtube.com/watch?v=-Vs9Vae2KI0
#https://www.youtube.com/watch?v=LTg-qP9iGFY&t=582s 
#https://www.youtube.com/watch?v=Pf7f67BuP2U

install.packages("neuralnet")
install.packages("MASS")
install.packages("caret")
library(MASS)
library(neuralnet)
library(caret)

#Boston dataset install,came with MASS package
dataFrame<-Boston

#Attributes explanation
help("Boston")
#Structure of dataframe
str(dataFrame)
#number of column and rows in dataframe
dim(dataFrame)

#Target variable histogram
hist(dataFrame$medv)
#Summary statistics of target variable
summary(dataFrame$medv)

#Getting min and max value
apply(dataFrame,2,range)

#Normalization
maxValue<-apply(dataFrame,2,max)
minValue<-apply(dataFrame,2,min)
dataFrame<- as.data.frame(scale(dataFrame,center=minValue, scale=maxValue-minValue))

head(dataFrame)

#Data Partitioning
"""ind <- sample(1:nrow(dataFrame),400)
trainDF<-dataFrame[ind,]
testDF<-dataFrame[-ind,]"""
set.seed(123)
ind<-createDataPartition(y=dataFrame$medv,p=.80,list=FALSE)
trainDF<-dataFrame[ind,]
testDF<-dataFrame[-ind,]

#Getting column names
allVars<-colnames(dataFrame) 
#Seperating predicting variables
predictorVars<- allVars[!allVars%in%"medv"]
#Formatting predictive variables 
predictorVars<-paste(predictorVars,collapse = "+")
#Create formula with target and predictive attributes
(form=as.formula(paste("medv~",predictorVars,collapse = "+")))

#Modelling Neural Network
neuralModel<-neuralnet(formula = form,
                       hidden = c(4,2),
                       linear.output = T ,
                       data = trainDF)
#Plotting Neural Model
plot(neuralModel)

#Predictions
predictions<-compute(neuralModel,testDF[,1:13])
str(predictions)
head(predictions$net.result)
head(testDF[,14])

#Comparision of real and predictive values
results <- data.frame(actual = testDF[,14], prediction = predictions$net.result)    
results

# Confusion Matrix & Misclassification Error - training set
output <- compute(neuralModel, trainDF[,-14])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, trainDF$medv)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - test set
output <- compute(neuralModel, testDF[,-14])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testDF$medv)
tab2
1-sum(diag(tab2))/sum(tab2)
