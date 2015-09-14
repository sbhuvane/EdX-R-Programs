stocks <- read.csv("StocksCluster.csv")
nrow(stocks)
# Proportion of PositiveDec
table(stocks$PositiveDec)
#max Correlation
sort(cor(stocks))

summary(stocks)

#Logistic Regression
library(caTools)
set.seed(144)

spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

stocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
predictTrain <- predict(stocksModel, newdata = stocksTrain, type = "response")
table(stocksTrain$PositiveDec, predictTrain > 0.5)
# Prediction on test data set
predictTest <- predict(stocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, predictTest > 0.5)


# Baseline accuracy of test data
table(stocksTest$PositiveDec)

#Clustering
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

library(caret)

preproc <- preProcess(limitedTrain)

normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

#K-means clustering

set.seed(144)
km <- kmeans(normTrain, center = 3)
table(km$cluster)

# Prediction using K-means

library(flexclust)

km.kcca <- as.kcca(km, normTrain)

clusterTrain <- predict(km.kcca)

clusterTest <- predict(km.kcca, newdata = normTest)

table(clusterTest)

# Map clusters to the original Stocks data

stocksTrain1 <- subset(stocksTrain, km$cluster == 1)
stocksTrain2 <- subset(stocksTrain, km$cluster == 2)
stocksTrain3 <- subset(stocksTrain, km$cluster == 3)

#use clusterTest to map clusters

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
nrow(stocksTest1)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# train logit on 3 train clusters
StocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# Predict using the logit model for each cluster against each test cluster

predictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
table(stocksTest1$PositiveDec, predictTest1 > 0.5)

predictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
table(stocksTest2$PositiveDec, predictTest2 > 0.5)

predictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")
table(stocksTest3$PositiveDec, predictTest3 > 0.5)

#Overall Accuracy

AllPredictions <- c(predictTest1,predictTest2,predictTest3)

AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)
