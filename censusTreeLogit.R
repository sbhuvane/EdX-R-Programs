census <- read.csv("census.csv")
str(census)
library(caTools)
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, spl == TRUE)
test <- subset(census, spl == FALSE)
#logistic regression model
logmod <- glm(over50k ~ ., data = train, family = "binomial")
summary(logmod)

predictTrain <- predict(logmod, newdata = test, type="response")
summary(predictTrain)
table(test$over50k, predictTrain > 0.5)
table(test$over50k)

library(ROCR)
ROCRpredTest <- prediction(predictTrain, test$over50k)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

library(rpart)
library(rpart.plot)
tree <- rpart(over50k ~ .,data = train, method = "class" )
prp(tree)


library(ROCR)
PredictROC <- predict(tree, newdata = test)
PredictROC
pred <- prediction(PredictROC[,2], test$over50k)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
AUC <- as.numeric(performance(pred, "auc")@y.values)
AUC
PredictTree <- predict(tree, newdata = test, type = "class")
table(test$over50k, PredictTree)


# Take a sample of 2000 from train to run randomforest
set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
library(randomForest)
forest <- randomForest(over50k ~ ., data = trainSmall)
PredictForest <- predict(forest, newdata = test)
table(test$over50k, PredictForest)

# Chart of how the variables are used for split in randomforest
vu = varUsed(forest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))

#impurity plot

varImpPlot(forest)

# Cross validation
library(caret)
library(e1071)

# 10 fold cross validation
numfolds <- trainControl(method="cv",number=10)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# cross validation using train function
train(over50k ~ ., data = train, method = "rpart", trControl = numfolds, tuneGrid = cartGrid)
#Use cp from the train function output
treeCV <- rpart(over50k ~ ., data = train, method = "class", cp=0.002)
PredictCV <- predict(treeCV, newdata = test, type = "class")
# confusion matrix
table(test$over50k, PredictCV)
# Plot the cross validated CART model
prp(treeCV)











