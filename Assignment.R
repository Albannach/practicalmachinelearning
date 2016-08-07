library(caret)
library(rattle)
library(randomForest)
#
# Data loading & Cleaning
#
# Read in the training and test datasets, setting to na all blanks and nulls etc.
trainingRaw = read.csv("pml-training.csv", header = TRUE, na.strings=c("","NA", "NULL", "#DIV/0!"))
testingRaw = read.csv("pml-testing.csv", header = TRUE, na.strings=c("","NA", "NULL", "#DIV/0!"))
#
# Remove the first seven columns (user name, timestamp etc. which are to do with the mangement of the data)
#
training0 = trainingRaw[,-(1:7)]
testing0 = testingRaw[,-(1:7)]
dim (training0); dim (testing0)
#
# Remove columns which aren't populated with data, i.e. those set to na when reading in
#
training1 = training0[, colSums(is.na(training0)) == 0]
testing1  = testing0[, colSums(is.na(training0)) == 0]
dim (training1); dim (testing1)
#
# Create a cross-validation set to use after the model has been built in order to test its accuracy.
#
set.seed(132435)
inTraining = createDataPartition(y=training1$classe, p=0.60, list=FALSE)
training2  = training1[inTraining,]
crossValidation2 = training1[-inTraining,]
dim(training2);  dim(crossValidation2)
#
# Find a remove those columns with either zero or near zero variance
#
nzv_cols <- nearZeroVar(training2)
if(length(nzv_cols) > 0) {
    training3 = training2[, -nzv_cols]
    crossValidation3 = crossValidation2[, -nzv_cols]
} else {
    training3 = training2
    crossValidation3 = crossValidation2
}
dim(training3);  dim(crossValidation3);
#
# Feature selection.
#
# Set a seed for the pseudo random number generator (to ensure repeatability) and
# fit a model to all of the variables using the random forest method of the train 
# function in the caret package..
#
set.seed(243513)
modelA_ = train(classe~.,
                data=training3,
                method="rf",
                trControl=trainControl(method="cv",number=2),
                prox=TRUE,
                verbose=TRUE,
                allowParallel=TRUE)
varImp(modelA_)
#
# Examine the importance of the various variables and select the top 10 most important. 
# Cutting down number of variables will help with producing a more easily understandable model
# 
set.seed(354213)
modelB_ = train(classe~roll_belt+pitch_belt+magnet_dumbbell_y+magnet_dumbbell_z+pitch_forearm+accel_dumbbell_y+roll_arm+roll_forearm,
                data=training3,
                method="rf",
                trControl=trainControl(method="cv",number=2),
                prox=TRUE,
                verbose=TRUE,
                allowParallel=TRUE)
#
# Look for the accuracy of the predicted values in the cross validation dataset.
#
predictions = predict(modelB_, newdata=crossValidation3)
confusionMatrix_ = confusionMatrix(predictions, crossValidation3$classe); confusionMatrix_
#
# Given an accuracy of 98%, accept this model and calculate the predictions against the test data (cleaned in the same way as the training data.)
#
testing1$classe = predict(modelB_, newdata=testing1)
#
# Print the predictions for the test data; these are entered into the Week 4 Quiz.
#
testing1[,"classe"]