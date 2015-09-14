airlines <- read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

#scaling the data to narmalize

library(caret)
preproc <- preProcess(airlines)

airlinesNorm <- predict(preproc, airlines)

summary(airlinesNorm)
# Mean is 0 for all variables (sd will be 1)

# Compute Distance between points to prepare for Hierarchical clustering
distance <- dist(airlinesNorm, method = "euclidean")

# Run Hierarchical Clustering

hierClustAir <- hclust(distance, method = "ward")

# Plot dendogram
plot(hierClustAir)

# Create 5 clusters

airClustGroups <- cutree(hierClustAir, k = 5)

# No. of rows in 1st cluster

table(airClustGroups)
#spl <- split(airlines, airClustGroups)
#nrow(spl[[1]])

# Average values of unnormalized data
tapply(airlines$Balance, airClustGroups, mean)
tapply(airlines$QualMiles, airClustGroups, mean)
tapply(airlines$BonusMiles, airClustGroups, mean)
tapply(airlines$BonusTrans, airClustGroups, mean)
tapply(airlines$FlightMiles, airClustGroups, mean)
tapply(airlines$FlightTrans, airClustGroups, mean)
tapply(airlines$DaysSinceEnroll, airClustGroups, mean)

#Alternative method
lapply(spl, colMeans)


# K Means clustering; k = 5
set.seed(88)
kclust <- kmeans(airlinesNorm, center = 5, iter.max = 1000)

table(kclust$cluster)

str(kclust)

kclust$centers
spl1 <- split(airlines, kclust$cluster)
lapply(spl1, colMeans)
str(spl1)
