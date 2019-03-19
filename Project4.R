#Load library
library(rpart)
library(caret)
library(corrplot)

#Data location
Train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Test  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#download data
training <- read.csv(url(Train))
testing  <- read.csv(url(Test))

TrainData  <- createDataPartition(training$classe, p=0.7, list=FALSE)
TrainSet <- training[TrainData, ]
TestSet <- training[-TrainData, ]
dim(TrainSet)
dim(TestSet)

NZV <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[, -NZV]
TestSet  <- TestSet[, -NZV]
dim(TrainSet)

# remove variables that are mostly NA
AllNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, AllNA==FALSE]
TestSet  <- TestSet[, AllNA==FALSE]
dim(TrainSet)
dim(TestSet)

TrainSet <- TrainSet[, -(1:5)]
TestSet  <- TestSet[, -(1:5)]
dim(TrainSet)
dim(TestSet)

corMatrix <- cor(TrainSet[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

set.seed(888)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRF <- train(classe ~ ., data=TrainSet, method="rf",
                          trControl=controlRF)
modFitRF$finalModel

predictRF <- predict(modFitRF, newdata=TestSet)
confMatRF <- confusionMatrix(predictRF, TestSet$classe)
confMatRF

plot(confMatRF$table, col = confMatRF$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confMatRF$overall['Accuracy'], 4)))

predictTEST <- predict(modFitRF, newdata=testing)
predictTEST