library(factoextra) # clustering algorithms & visualization
library(gridExtra)
data <- read.table("iris.txt", header=TRUE)

cluster1 <- kmeans(data[,1:4], centers=2, iter.max = 100)
cluster2 <- kmeans(data[,1:4], centers=3, iter.max = 100)
cluster3 <- kmeans(data[,1:4], centers=4, iter.max = 100)
cluster4 <- kmeans(data[,1:4], centers=5, iter.max = 100)

#print(cluster)
print(cluster2$cluster)
#print(sum(cluster1$withinss))
mapping <- c()
for (i in data[,5]) {
  if (i == "setosa") {mapping <- c(mapping, 2)}
  if (i == "versicolor") {mapping <- c(mapping, 3)}
  if (i == "virginica") {mapping <- c(mapping, 1)}
}
print(mapping)
acc <- sum(cluster2$cluster == mapping) / nrow(data)

plot1 <- fviz_cluster(cluster1, data = data[,1:4]) + ggtitle("k = 2")
plot2 <- fviz_cluster(cluster2, data = data[,1:4]) + ggtitle("k = 3")
plot3 <- fviz_cluster(cluster3, data = data[,1:4]) + ggtitle("k = 4")
plot4 <- fviz_cluster(cluster4, data = data[,1:4]) + ggtitle("k = 5")

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)
