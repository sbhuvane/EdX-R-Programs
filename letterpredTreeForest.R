letters <- read.csv("letters_ABPR.csv")
str(letters)
# create a new logical factor isB
letters$isB <-  as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
trainB <- subset(letters, split == TRUE)
testB <- subset(letters, split == FALSE)
table(testB$isB)

#CART Model
CARTB <- rpart(isB ~ .-letter, data = trainB, method = "class")
prp(CARTB)
predictB <- predict(CARTB, newdata = testB, type = "class")
table(testB$isB, predictB)


#Random forest model
library(randomForest)
set.seed(1000)
forestB <- randomForest(isB ~ .-letter, data = trainB)
predForB <- predict(forestB, newdata = testB)
table(testB$isB, predForB)

# Prediction for A, B, P and R
letters$letter <-  as.factor(letters$letter)
library(caTools)
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)
table(test$letter)
nrow(test)
str(train)

#CART Model
CARTm <- rpart(letter ~ .-isB, data = train, method = "class")
prp(CARTm)
predictm <- predict(CARTm, newdata = test, type = "class")
table(test$letter, predictm)

#Random forest model
library(randomForest)
set.seed(1000)
forestm <- randomForest(letter ~ .-isB, data = train)
predForm <- predict(forestm, newdata = test)
table(test$letter, predForm)










