polling <- read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)
#Install mice package to run multiple implutation of NAs
install.packages("mice")
library("mice")
# Create a simple data frame of polling related columns
simple <- polling[,3:6]
str(simple)
# Another way is simple <- polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
simple <- polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
summary(simple)
set.seed(144)
#run multiple imputation
imp <- mice(simple)
imputed <- complete(imp, action = 1)
summary(imputed)
