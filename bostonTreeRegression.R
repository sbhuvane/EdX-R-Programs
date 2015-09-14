boston <- read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT)
# Highlight points closer to Charles river 
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19)
# Highlight MIT which is in census TRACT 3531
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", pch = 19)
# Plot NOX pollution
summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col = "green", pch = 19)
# Plot Median Value - afresh
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "green", pch = 19)

# Build a linear regression using Lon and Lat and use plot to see how it matches with the actuals
latlonlm <- lm(MEDV ~ LON + LAT, data = boston)
summary(latlonlm)
# plot lm values that are > median to see how they match
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col = "blue", pch = "$")

# Build a regression Tree
library(rpart)
library(rpart.plot)
# No type = "class" as we are building a regression tree
latlontree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
# Plot the model predictions
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "green", pch = 19)
fittedvalues <- predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col = "blue", pch = "$")
# Rebuild the Regression Tree using minbucket = 50
latlontree <- rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
# Plot the decision tree
plot(latlontree)
text(latlontree)
# Plot the right side (lower price) of the decision tree in the actual lat lon plot
plot(boston$LON, boston$LAT)
# Plot the lon (vertical line) corresponding to first split
abline(v = -71.07)
#Plot tje second decision which is lat of 42.17 (horizonta line)
abline (h = 42.17)
# Plot the third decision which is lat of 42.21
abline (h = 42.21)
# Plot the points > median values
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "green", pch = 19)

#Linear Regression Vs Regression Tree
library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
train <- subset(boston, split = TRUE)
test <- subset(boston, split = FALSE)
# Build linear regression model
linreg <- lm(MEDV ~ LAT + LON +  CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)
linreg.pred <- predict(linreg, newdata = test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2)
linreg.sse

# build Regression tree Model
tree <- rpart(MEDV ~ LAT + LON +  CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
tree.pred <- predict(tree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse

# Cross Validation
library(caret)
library(e1071)

# 10 fold cross validation
tr.control <- trainControl(method="cv",number=10)
cpGrid <- expand.grid(.cp= (0:10)*0.001)
# cross validation using train function
tr <- train(MEDV ~ LAT + LON +  CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cpGrid)
tr
# Model based on the CP chosen from the train function
best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
