pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
summary(pisaTrain)
str(pisaTrain)
summary(pisaTest)
tapply(pisaTrain$readingScore,pisaTrain$male, mean)
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)
str(pisaTest)
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")
lmScore <- lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
SSE <- sum(lmScore$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE
#Alternative way to get RMSE
RMSEa <- sqrt(mean(lmScore$residuals^2))
RMSEa
predTest <- predict(lmScore, newdata = pisaTest)
summary(predTest)
max(predTest) - min(predTest)
SSEP <- sum((predTest - pisaTest$readingScore)^2)
SSEP
SSTP <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SSTP
R2P <- 1 - SSEP/SSTP
R2P
RMSEP <- sqrt(SSEP/nrow(pisaTest))
RMSEP
#Alternative way to get RMSE
RMSEPa <- sqrt(mean((predTest - pisaTest$readingScore)^2))
RMSEPa
# Predicted baseline score is mean(pisaTrain$readingScore)
mean(pisaTrain$readingScore)
