gerber <- read.csv("gerber.csv")
str(gerber)
table(gerber$voting)
table(gerber$voting, gerber$hawthorne)
table(gerber$voting, gerber$civicduty)
table(gerber$voting, gerber$neighbors)
table(gerber$voting, gerber$self)
# Alternate way to find proportion
tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)
#logistic regression model
logmod <- glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family = binomial)
summary(logmod)
predlogmod <- predict(logmod, type="response")
table(gerber$voting, predlogmod > 0.3)
table(gerber$voting, predlogmod > 0.5)
library(ROCR)
ROCRpred <- prediction(predlogmod, gerber$voting)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc
# Tree Model
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

treepred <- predict(CARTmodel2, newdata = gerber)
treepred
tapply(treepred, gerber$civicduty, mean)

CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
tapply(treepred, gerber$control & gerber$sex, mean)
tapply(treepred, gerber$civicduty & gerber$sex, mean)
# Regression Tree for Control group only
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4)
treepred4 <- predict(CARTmodel4, newdata = gerber)
pred4 <- tapply(treepred4, gerber$control, mean)
pred4[1] - pred4[2]
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

# Alternate solution
CARTcontrol <- rpart(voting ~ control, data=gerber, cp=0.0)

CARTsex <- rpart(voting ~ control + sex, data=gerber, cp=0.0)

prp(CARTcontrol, digits=6)

# Logistic regression with sex

logmod2 <- glm(voting ~ control + sex, data = gerber, family = binomial)
summary(logmod2)

#create data frame containing only control and sex
Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logmod2, newdata=Possibilities, type="response")


logmod3 <- glm(voting ~ control + sex + sex:control, data = gerber, family = binomial)
summary(logmod3)
predict(logmod3, newdata=Possibilities, type="response")
