## Simple illustratioin of Hierarchical clustering with iris dataset

data(iris)

# 10% data used 
index <- sample(1:nrow(iris), 0.1*nrow(iris))
iris.sample <- iris[index,]

# Delete the labels
iris.sample$Species <- NULL

# Use hclust() with the linkage method of single.
hc <- hclust(dist(iris.sample), method="single")

# Plot the dendrogram with labels to the species.
# Split the dendrogram into 3 clusters.
plot(hc, labels=iris$Species[index])
rect.hclust(hc, k=3) 

# Split the dendrogram into 4 clusters.
plot(hc, labels=iris$Species[index])
rect.hclust(hc, k=4)

# Split the dendrogram into 5 clusters.
plot(hc, labels=iris$Species[index])
rect.hclust(hc, k=5)


