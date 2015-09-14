movies <- read.table("movieLens.txt", header = FALSE, sep ="|", quote = "\"")
str(movies)
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
movies$ID <- NULL
#movies1 <- movies[,-c(2,3,4)]
#str(movies1)
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
str(movies)
#Remove duplicate entries
movies <- unique(movies)
str(movies)
#no. of comedies
table(movies$Comedy)
# no. of western
table(movies$Western)
# no. of romance AND drama
table(movies$Romance, movies$Drama)
# Hierarchical clustering
distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward")
plot(clusterMovies)
# Let us have 10 clusters
clusterGroups <- cutree(clusterMovies, k = 10)
# Let us find more about the groups - How many action genre movies are in each cluster
tapply(movies$Action, clusterGroups, mean)

# Alternate way to find which genre dominates a cluster...run the below command for 10 cluster
colMeans(subset(movies[2:20], clusterGroups == 1))
colMeans(subset(movies[2:20], clusterGroups == 2))

# Most simple method to get the same result as above
spl <- split(movies[2:20], clusterGroups)
str(spl)
#spl is list of 10 (in this case) data frames
# Cluster 1 is
head(spl[[1]])
# The above command is equivalent to subset(movies[2:20], clusterGroups == 1)
# Find the dominating genre in each cluster

lapply(spl, colMeans)

#Find which cluster the movie "Men in Black (1997)" belongs to:
subset(movies, Title == "Men in Black (1997)")
# It is in 257th record in movies dataframe
clusterGroups[257]

#Movies in CLuster 2
cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

# Quick Question - Group the movies into 2 clusters
clusterGroup2 <- cutree(clusterMovies, k = 2)
#cluster2m <- subset(movies, clusterGroup2 == 2)
#colMeans(cluster2m)
colMeans(subset(movies[2:20], clusterGroup2 == 2))
