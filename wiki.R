wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)


library(tm)

corpusAdded <- Corpus(VectorSource(wiki$Added))

corpusAdded <- tm_map(corpusAdded, PlainTextDocument)

corpusAdded = tm_map(corpusAdded, removePunctuation)

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument, "en")

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

# Same for Removed

corpusRemoved <- Corpus(VectorSource(wiki$Removed))

corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)

corpusRemoved = tm_map(corpusRemoved, removePunctuation)

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument, "en")

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved


wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))
str(wordsRemoved)

wikiWords <- cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal <- wiki$Vandal

#Split data into train and test in 70-30 ratio
library(caTools)
set.seed(123)
spl <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)

trainWiki <- subset(wikiWords, spl == TRUE)
testWiki <- subset(wikiWords, spl == FALSE)

table(testWiki$Vandal)

#Use CART model to predict the sentiment
library(rpart)
library(rpart.plot)

wikiCART <- rpart(Vandal~., data=trainWiki, method="class")
prp(wikiCART)

predictWiki <- predict(wikiCART, newdata = testWiki, type = "class")

table(testWiki$Vandal, predictWiki)

#Question 2

wikiWords2 <- wikiWords

wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE),1,0)

table(wikiWords2$HTTP)

wikiTrain2 <- subset(wikiWords2, spl == TRUE)
wikiTest2 <- subset(wikiWords2, spl == FALSE)


wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method = "class")

predictWiki2 <- predict(wikiCART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predictWiki2)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)


wikiTrain2 <- subset(wikiWords2, spl == TRUE)
wikiTest2 <- subset(wikiWords2, spl == FALSE)


wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method = "class")

predictWiki2 <- predict(wikiCART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predictWiki2)


wikiWords3 <- wikiWords2

wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiTrain3 <- subset(wikiWords3, spl == TRUE)
wikiTest3 <- subset(wikiWords3, spl == FALSE)


wikiCART3 <- rpart(Vandal~., data=wikiTrain3, method = "class")

predictWiki3 <- predict(wikiCART3, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal, predictWiki3)

prp(wikiCART3)








