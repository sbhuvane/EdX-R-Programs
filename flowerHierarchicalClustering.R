flower <- read.csv("flower.csv", header = FALSE)
str(flower)
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)
distance <- dist(flowerVector, method = "euclidean")
str(distance)
clusterIntensity <- hclust(distance, method = "ward")
plot(clusterIntensity)
#Plot rect clusters in the abpve plot
rect.hclust(clusterIntensity, k = 3, border = "red")

flowerClusters <- cutree(clusterIntensity, k = 3)
flowerClusters

tapply(flowerVector, flowerClusters, mean)

# to create the impagebak, convert the vector into 50 x 50 matrix using dim function
dim(flowerClusters) <- c(50,50)

image(flowerClusters, axis = FALSE)
# Original image on a grayscale
image(flowerMatrix, axis = FALSE, col = grey(seq(0,1,length = 256)))
