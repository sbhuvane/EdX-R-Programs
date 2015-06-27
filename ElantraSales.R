elantra <- read.csv("elantra.csv")
str(elantra)
elantra_train <- elantra[elantra$Year <= 2012,]
str(elantra_train)
elantra_test <- elantra[elantra$Year > 2012,]
str(elantra_test)
# LM for Sales using Unemployment, CPI_all, CPI_energy & Queries
sales <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data = elantra_train)
summary(sales)
sales2 <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data = elantra_train)
summary(sales2)
# create a month factor variable using as.factor
mfact <- as.factor(elantra_train$Month)
elantra_train$mfact <- mfact
sales3 <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + mfact, data = elantra_train)
summary(sales3)
# Correlation to check multi-colinearity
cor(elantra_train$CPI_energy, elantra_train$Month)
cor(elantra_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
sales4 <- lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + mfact, data = elantra_train)
summary(sales4)
# Predict using Test data
elantra_test$mfact <- as.factor(elantra_test$Month)
salespred <- predict(sales4, newdata = elantra_test)
SSE <- sum((salespred - elantra_test$ElantraSales)^2)
SSE
# baseline is mean of elantra train
mean(elantra_train$ElantraSales)
SST <- sum((mean(elantra_train$ElantraSales) - elantra_test$ElantraSales)^2)
SST
R2 <- 1 - SSE/SST
R2
#largest abs error in predicted
max(abs(salespred - elantra_test$ElantraSales))
elantra_test[which.max(abs(salespred - elantra_test$ElantraSales)),]
