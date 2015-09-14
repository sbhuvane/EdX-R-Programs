stevens <- read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
split <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, split == TRUE)
Test <- subset(stevens, split == FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
# Build classification tree using rpart
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25)
prp(StevensTree)
PredictCART <- predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
library(ROCR)
PredictROC <- predict(StevensTree, newdata = Test)
PredictROC
pred <- prediction(PredictROC[,2], Test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
AUC <- as.numeric(performance(pred, "auc")@y.values)
AUC
# CART with minbucket = 5
StevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 5)
prp(StevensTree2)
# CART with minbucket = 100
StevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 100)
prp(StevensTree3)
# Random Forset implementation
library(randomForest)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)

StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest <- predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
# Set seed to 100
set.seed(100)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest <- predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)

# Set seed to 200
set.seed(200)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest <- predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)


# Cross-Validation for CART Model
# Install packages caret & e1071
library(caret)
library(e1071)

# 10 fold cross validation
numfolds <- trainControl(method="cv",number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
# cross validation using train function
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numfolds, tuneGrid = cpGrid)
#Use cp from the train function output
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp=0.18)
PredictCV <- predict(StevensTreeCV, newdata = Test, type = "class")
# confusion matrix
table(Test$Reverse, PredictCV)
# Plot the cross validated CART model
prp(StevensTreeCV)
