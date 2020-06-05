
#scale data, so that every attribute has the same weight
scale_data <- function(df) {
  scaled_data <- as.data.frame(scale(df))
  scaled_data
}

#calculate the best number of clusters
calculate_k <- function(df) {
  res <- NbClust(data = df, diss = NULL, distance="euclidean",
                 min.nc=2, max.nc=10, method="kmeans", index="all")
  k <- max(res$Best.partition)
  k
}

#get cluster with K-means
get_kmeans_cluster <- function(df) {
  set.seed(123)
  k <- calculate_k(df)
  km.out <- kmeans(df, k, nstart=10)
  km.out
}

#do everything to get the cluster
get_cluster <- function(df) {
  df <- scale_data(df)
  
  cluster <- get_kmeans_cluster(df)
  cluster
}