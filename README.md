# CourseraML
Coursera Practical Machine Learning Module 4

# Load required libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(e1071)


#Load the data
training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Read and clean the data. Remove NA and useless columns
training <- read.csv(training_url, na.strings = c("NA", "#DIV/0!", ""))
training<-training[,colSums(is.na(training)) == 0]
training   <-training[,-c(1:7)]
testing  <- read.csv(testing_url, na.strings = c("NA", "#DIV/0!", ""))
testing <-testing[,colSums(is.na(testing)) == 0]
testing <-testing[,-c(1:7)]


#Split the data to 70% for training and 30% for validation
set.seed(123)
inTrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
trainSet <- training[inTrain, ]
validateSet <- training[-inTrain, ]

#Plot the training set.
barplot(table(trainSet$classe))


#Build the model
modelFit <- train(classe ~ ., data=trainSet, method="rf", trControl=trainControl(method="cv", number=5), ntree = 100)

#Validate the model
predictions <- predict(modelFit, validateSet)
validateSetClass <-  as.factor(validateSet$classe)
confusionMatrix(predictions, validateSetClass)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1673    5    0    0    0
         B    1 1127    5    0    0
         C    0    7 1020   10    5
         D    0    0    1  954    6
         E    0    0    0    0 1071

Overall Statistics
                                          
               Accuracy : 0.9932          
                 95% CI : (0.9908, 0.9951)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9914          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9994   0.9895   0.9942   0.9896   0.9898
Specificity            0.9988   0.9987   0.9955   0.9986   1.0000
Pos Pred Value         0.9970   0.9947   0.9789   0.9927   1.0000
Neg Pred Value         0.9998   0.9975   0.9988   0.9980   0.9977
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2843   0.1915   0.1733   0.1621   0.1820
Detection Prevalence   0.2851   0.1925   0.1771   0.1633   0.1820
Balanced Accuracy      0.9991   0.9941   0.9948   0.9941   0.9949



#Apply to test
test_predictions <- predict(modelFit, testing)
test_predictions
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
