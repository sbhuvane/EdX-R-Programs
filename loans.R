loans <- read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)
summary(loans)
# Number of rows after NAs are removed
nrow(subset(loans, !is.na(loans$log.annual.inc) & !is.na(loans$days.with.cr.line) & !is.na(loans$revol.util) & !is.na(loans$inq.last.6mths) & !is.na(loans$delinq.2yrs) & !is.na(loans$pub.rec)))
# Imputing missing values using mice
library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed
# Load the imputed dataset into loans
loans <- read.csv("loans_imputed.csv")
summary(loans)
# Split the data into train and test

library(caTools)
set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, split == TRUE)
test <- subset(loans, split == FALSE)
str(train)
str(test)
# Build logistic regression model using all independent variables
loansmod <- glm(not.fully.paid ~ ., data = train, family = "binomial")
summary(loansmod)
# Predict on test data
predicted.risk <- predict(loansmod, type="response", newdata = test)
table(test$not.fully.paid, predicted.risk > 0.5)
#Baseline is not default
table(test$not.fully.paid)
# AUC computation using ROCR
ROCRpred <- prediction(predicted.risk, test$not.fully.paid)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

# bivariate model using int.rate
loansmod2 <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(loansmod2)
# predict using the bivariate model
predicted.risk2 <- predict(loansmod2, type="response", newdata = test)
max(predicted.risk2)
# confusion matrix
table(test$not.fully.paid, predicted.risk2 > 0.5)

#AUC for the bivariate model
pred <- prediction(predicted.risk2, test$not.fully.paid)
as.numeric(performance(pred,"auc")@y.values)

# Interest compounded = exp(rt) where r is interest rate and t is time period

# for a $1 investment profit is exp(rt) - 1

test$profit <- exp(test$int.rate * 3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)
sum(test$profit)/nrow(test)

# select loans with int rate > 15%

highinterest <- subset(test, int.rate >= 0.15)
str(highinterest)
mean(highinterest$profit)

table(highinterest$not.fully.paid)

# Add predicted risk to the test data
test$predicted.risk <- predicted.risk
str(test)

highinterest <- subset(test, int.rate >= 0.15)
str(highinterest)

# get the 100th highest predicted risk

cutoff <- sort(highinterest$predicted.risk, decreasing=FALSE)[100]

# select loans from high interest pool that has risk less than the above cutoff

selectedLoans <- subset(highinterest, predicted.risk <= cutoff)
nrow(selectedLoans)
summary(selectedLoans)
sum(selectedLoans$profit)

# try the model without log income
loans1 <- loans
str(loans1)
annual.inc <- exp(loans1$log.annual.inc)
loans1$log.annual.inc <- annual.inc
library(caTools)
set.seed(144)
split <- sample.split(loans1$not.fully.paid, SplitRatio = 0.7)
train1 <- subset(loans1, split == TRUE)
test1 <- subset(loans1, split == FALSE)

# Build logistic regression model using all independent variables
loansmod1 <- glm(not.fully.paid ~ ., data = train1, family = "binomial")
summary(loansmod1)
predicted.risk1 <- predict(loansmod1, type="response", newdata = test1)
table(test1$not.fully.paid, predicted.risk1 > 0.5)
ROCRpred1 <- prediction(predicted.risk1, test1$not.fully.paid)
auc <- as.numeric(performance(ROCRpred1, "auc")@y.values)