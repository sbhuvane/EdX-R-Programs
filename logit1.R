quality <- read.csv("quality.csv")
str(quality)
#Poor Care is 1 anf Good Care is 0 in the data; 
table(quality$PoorCare)
#Baseline model is freq of good care (98/131 ~ 75%)
# Install caTools package
install.packages("caTools")
#Load the installed package
library("caTools")
set.seed(88)
# Split 75% of data using Poor Care variable
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
# +ve coefficients means it influences heiger P(y=1)
predictTrain <- predict(QualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
# Build a model using "StartedOnCombination" and "ProviderCount"
QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)
#confusion Matrix
table(qualityTrain$PoorCare, predictTrain > 0.5)
# Build ROC using the ROCR library functions
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
#add color to the ROC plot
plot(ROCRperf, colorize = TRUE)
# add threshold values to the plot
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))
# Predict on test data set
predictTest <- predict(QualityLog, type = "response", newdata = qualityTest)
# confusion Matrix at threshold value of 0.3
table(qualityTest$PoorCare, predictTest > 0.3)
# AUC for the prediction
ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
