trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
summary(trials)
max(nchar(trials$abstract))
sum(nchar(trials$abstract) == 0)
min(nchar(trials$title))
trials[nchar(trials$title)==min(nchar(trials$title)),]
# Alternate
which.min(nchar(trials$title))



library(tm)

corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument, "en")
corpusAbstract = tm_map(corpusAbstract, stemDocument, "en")

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

# no. of terms left in Title 
ncol(dtmTitle)
str(dtmTitle)

ncol(dtmAbstract)

#Most Frequent term in Abstract
which.max(colSums(dtmAbstract))


colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

str(dtmTitle)
str(dtmAbstract)


dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
ncol(dtm)


library(caTools)
set.seed(144)

spl <- sample.split(dtm$trial, SplitRatio = 0.7)

train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)

table(train$trial)

library(rpart)
library(rpart.plot)

trialCART <- rpart(trial ~ ., data = train, method = "class")
prp(trialCART)

#training set predicted probabilities
predTrain <- predict(trialCART, newdata=train)
predTrain[1:10,]
predTrain.prob <- predTrain[,2]
max(predTrain.prob)
# alternatively
summary(predTrain)
head(train$trial)
nrow(predTrain)


table(train$trial, predTrain[,2] > 0.5)


predTest <- predict(trialCART, newdata=test)

table(test$trial, predTest[,2] >0.5)

str(predTest)

# ROC curve

library(ROCR)

predROCR = prediction(predTest[,2], test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



