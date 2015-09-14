emails <- read.csv("emails.csv", stringsAsFactors = FALSE)
nrow(emails)
str(emails)
table(emails$spam)

max(nchar(emails$text))
min(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus <- tm_map(corpus, stemDocument, "en")

dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm, 0.95)
spdtm


emailsSparse <- as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

nchar(emailsSparse)
which(colSums(emailsSparse) >= 5000)
str(emailsSparse)
nrow(emailsSparse[emailsSparse$spam == 0,])

table(emailsSparse$spam == 0)

sum(colSums(subset(emailsSparse, spam == 0)) >= 5000)

sort(colSums(subset(emailsSparse, spam == 1)))


emailsSparse$spam <- as.factor(emailsSparse$spam)

library(caTools)

set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

spamLog <- glm(spam ~ ., data = train, family = "binomial")

spamCART <- rpart(spam ~ ., data = train, method = "class")

#Random forest model
library(randomForest)
set.seed(123)

spamRF <- randomForest(spam ~ ., data = train)

predLog <- predict(spamLog)
table(predLog < 0.00001)
table(predLog > 0.99999)
table (predLog > 0.00001)

predTrainCART <- predict(spamCART)[,2]
predTrainRF <- predict(spamRF, type = "prob")[,2]

summary(spamLog)
prp(spamCART)

table(train$spam, predLog > 0.5)
library(ROCR)
ROCRpredTrainLog <- prediction(predLog, train$spam)
auc <- as.numeric(performance(ROCRpredTrainLog, "auc")@y.values)
auc

table(train$spam, predTrainCART > 0.5)
library(ROCR)
ROCRpredTrainCART <- prediction(predTrainCART, train$spam)
auc <- as.numeric(performance(ROCRpredTrainCART, "auc")@y.values)
auc

table(train$spam, predTrainRF > 0.5)
library(ROCR)
ROCRpredTrainRF <- prediction(predTrainRF, train$spam)
auc <- as.numeric(performance(ROCRpredTrainRF, "auc")@y.values)
auc


predLog <- predict(spamLog, newdata = test, type= "response")
table(test$spam, predLog > 0.5)
ROCRpredTestLog <- prediction(predLog, test$spam)
auc <- as.numeric(performance(ROCRpredTestLog, "auc")@y.values)
auc

predTestCART <- predict(spamCART, newdata = test)[,2]
predTestRF <- predict(spamRF, newdata = test, type = "prob")[,2]

table(test$spam, predTestCART > 0.5)
ROCRpredTestCART <- prediction(predTestCART, test$spam)
auc <- as.numeric(performance(ROCRpredTestCART, "auc")@y.values)
auc

table(test$spam, predTestRF > 0.5)
ROCRpredTestRF <- prediction(predTestRF, test$spam)
auc <- as.numeric(performance(ROCRpredTestRF, "auc")@y.values)
auc




