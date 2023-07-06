# load the required packages
library(caret); 
library(rattle); 
library(rpart); 
library(rpart.plot)
library(randomForest); 
library(repmis)

# import the data 

training <- read.csv("D:/Infosys/pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("D:/Infosys/pml-testing.csv", na.strings = c("NA", ""))

training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]

set.seed(7826) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]

control <- trainControl(method = "cv", number = 5)
fit_rpart <- train(classe ~ ., data = train, method = "rpart", 
                   trControl = control)
print(fit_rpart, digits = 4)

fancyRpartPlot(fit_rpart$finalModel)

predict_rpart <- predict(fit_rpart, valid)
# Show prediction result
(conf_rpart <- confusionMatrix(valid$classe, predict_rpart))

(accuracy_rpart <- conf_rpart$overall[1])

fit_rf <- train(classe ~ ., data = train, method = "rf", 
                trControl = control)
print(fit_rf, digits = 4)

# predict outcomes using validation set
predict_rf <- predict(fit_rf, valid)
# Show prediction result
(conf_rf <- confusionMatrix(valid$classe, predict_rf))

(accuracy_rf <- conf_rf$overall[1])

(predict(fit_rf, testData))


