tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
#Create a negative sentiment indicator
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
#install tm package
install.packages("tm")
library("tm")
install.packages("SnowballC")
library(SnowballC)
#create a 'corpus' of tweets
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
# convert tweets to lower-case
corpus <- tm_map(corpus, tolower)
corpus[[1]]
#convert corpus documents into text document format
corpus <- tm_map(corpus, PlainTextDocument)
corpus
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]
# std. stopwords provided in R
stopwords("english")[1:10]
# remove stopwords and 'apple' from the tweets
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]
# Stemming
corpus <- tm_map(corpus, stemDocument, "en")
corpus[[1]]

#bag of words - count of words in each tweet
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])
# Find words that appear most frequently
findFreqTerms(frequencies, lowfreq = 20)
# Remove words that have low freq; Retain words that appear in 0.5% of tweets
sparse <- removeSparseTerms(frequencies, 0.995)
str(sparse)
#Convert the sparse which is a list into matrix and then into a data frame
tweetsSparse <- as.data.frame(as.matrix(sparse))
head(tweetsSparse)

# some of the colnames start which numbers which are difficult to process in R;
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
# The above command needs to be done for all text analytics data frames

tweetsSparse$Negative <- tweets$Negative

#Split data into train and test in 70-30 ratio
library(caTools)
set.seed(123)
spl <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse <- subset(tweetsSparse, spl == TRUE)
testSparse <- subset(tweetsSparse, spl == FALSE)

# Quick Questions - Find terms that occur at least 100 time

findFreqTerms(frequencies, lowfreq = 100)

#Use CART model to predict the sentiment
library(rpart)
library(rpart.plot)
tweetCART <- rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)

#Predict for the test data
predictCART <- predict(tweetCART, newdata = testSparse, type = "class")

#confusion Matrix
table(testSparse$Negative, predictCART)
# Accuracy is 0.88
# Baseline is default Negative
table(testSparse$Negative)
#baseline accuracy is 0.84

# Build randomForest model
library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data = trainSparse)
predictRF <- predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)


#Quick Question - Build logistic regression model
tweetLM <- glm(Negative ~ ., data = trainSparse, family = "binomial")
predictLM <- predict(tweetLM, newdata = testSparse, type = "response")
table(testSparse$Negative, predictLM > 0.5)





