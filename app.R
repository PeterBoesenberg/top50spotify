library(data.table)
library(NbClust)
library(plotly)
source("scripts/load.R")
source("scripts/cluster.R")

dt <- load_data()
cluster_data <- get_cluster(select_features(dt))
cluster <- cluster_data$centers
str(get_cluster(dt))
dt <- dt[, cluster:=cluster_data$cluster][order(-cluster)]


cluster <- as.data.table(cluster)
names <- names(cluster)
cluster <- transpose(cluster)
colnames(cluster) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7", "Cluster8")
cluster[,attributes:=names]

# visualize cluster and attributes
get_chart <- function () {
  fig <- plot_ly(data=cluster,
                 
                 type="scatter",
                 mode = 'box',
                 orientation = 'v')
  fig <- fig %>% add_trace(y = ~Cluster1,x=~attributes, name="Cluster 1")
  fig <- fig %>% add_trace(y = ~Cluster2,x=~attributes, name="Cluster 2")
  fig <- fig %>% add_trace(y = ~Cluster3,x=~attributes, name="Cluster 3")
  fig <- fig %>% add_trace(y = ~Cluster4,x=~attributes, name="Cluster 4")
  fig <- fig %>% add_trace(y = ~Cluster5,x=~attributes, name="Cluster 5")
  fig <- fig %>% add_trace(y = ~Cluster6,x=~attributes, name="Cluster 6")
  fig <- fig %>% add_trace(y = ~Cluster7,x=~attributes, name="Cluster 7")
  fig <- fig %>% add_trace(y = ~Cluster8,x=~attributes, name="Cluster 8")

  fig
}

get_chart()

# any correlation between cluster and Genre?
aov1 = aov(dt[,cluster] ~ dt[,factor(Genre)])
summary(aov1)
# doesn't look like it

# lets try a multiple regression
fit <- lm(dt[,cluster] ~ dt[,factor(Genre)], data=dt)
summary(fit)
# only two intercept significant, but only on 0.1 level - meh