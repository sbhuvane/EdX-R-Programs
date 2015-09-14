songs <- read.csv("songs.csv")
str(songs)
nrow(subset(songs, year==2010))
nrow(subset(songs, artistname == "Michael Jackson"))
MJ <- subset(songs, artistname == "Michael Jackson")
# Top 10 MJ songs
(subset(MJ, Top10 == 1))$songtitle
# values of timesignature
table(songs$timesignature)
# Song that has the highest tempo
# Method 1 - get the max of the songs listed
tapply(songs$tempo, songs$songtitle == 'My Happy Ending' | songs$songtitle == 'Until The Day I Die' | songs$songtitle == 'You Make Me Wanna...' | songs$songtitle == "Wanna Be Startin' Somethin'", max)
# find the song title which has the max tempo
songs$songtitle[songs$tempo == 244.307]
#method 2 - find the index number of the song with max tempo
which.max(songs$tempo)
#Find the song title using the index number
songs$songtitle[which.max(songs$tempo)]
# Split songs into training and test data set; All songs before 2010 in Train;
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
str(SongsTrain)
str(SongsTest)
# Non-variables that we want to remove from the data frame as they are not needed for the model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest <- SongsTest[,!(names(SongsTest) %in% nonvars)]
# Build logistic model using all the variables
SongsLog1 <- glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)
# Correlation in the training set
cor(SongsTrain)
cor(SongsTrain$loudness, SongsTrain$energy)
#  Build Logistic model without Loudness
SongsLog2 <- glm(Top10 ~ .-loudness, data = SongsTrain, family = binomial)
summary(SongsLog2)
#  Build Logistic model without Energy
SongsLog3 <- glm(Top10 ~ .-energy, data = SongsTrain, family = binomial)
summary(SongsLog3)
# Predict using Model3 and calc accuracy with a threshold of 0.45
predictTest <- predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, predictTest >= 0.45)








