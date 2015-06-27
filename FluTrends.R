FluTrain <- read.csv("FluTrain.csv")
str(FluTrain)
str(tapply(FluTrain$ILI, FluTrain$Week, max))
max(FluTrain$ILI)
FluTrain[FluTrain$ILI == max(FluTrain$ILI),]
head(FluTrain)
FluTrain[FluTrain$Queries == max(FluTrain$Queries),]
max(FluTrain$Queries)
plot(FluTrain$ILI)
hist(FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))
FluModel <- lm(log(ILI) ~ Queries, data= FluTrain)
summary(FluModel)
c1 <- cor(FluTrain$Queries,log(FluTrain$ILI))
c1^2
log(1/c1)
exp(-0.5*c1)
FluTest <- read.csv("FluTest.csv")
PredTest1 <- exp(predict(FluModel, newdata = FluTest))
summary(PredTest1)
FluTest[FluTest$Week == "2012-03-11 - 2012-03-17",]
head(FluTest)
PredTest1[11]
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]
RMSE <- sqrt(mean((PredTest1 - FluTest$ILI)^2))
RMSE
ILILag2 <- lag(zoo(FluTrain$ILI),-2, na.pad=TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)
summary(FluTrain$ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
FluTest$ILILag2 <- coredata(lag(zoo(FluTest$ILI),-2, na.pad=TRUE))
summary(FluTest$ILILag2)
FluTest$ILILag2[1:2]
FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain) - 1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]
PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
summary(PredTest2)
RMSE <- sqrt(mean((PredTest2 - FluTest$ILI)^2))
RMSE