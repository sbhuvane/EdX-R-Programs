parole <- read.csv("parole.csv")
str(parole)
#number of parole violators
table(parole$violator)
summary(parole)
# convert State and Crime as factor as they have more than 3 levels of unordered factors
state <- as.factor(parole$state)
crime <- as.factor(parole$crime)
parole$state <- state
parole$crime <- crime
summary(parole)
# Split data into train and test 
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)
str(parole)
str(train)
str(test)
summary(train)
# Logistic model using all variables
violatorLog <- glm(violator ~ ., data = train, family = "binomial")
summary(violatorLog)
# Predict for the test data
predictTest <- predict(violatorLog, type = "response", newdata = test)
max(predictTest)
table(test$violator, predictTest>0.5)
#Find AUC
ROCRpred <- prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
