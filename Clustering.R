library(tidyverse)
library(class)
library(factoextra)


bin_data <- read.csv("C:/Users/misherman/DS-520/dsc520/data/binary-classifier-data.csv")
bin_data$label <- as.factor(bin_data$label)

ggplot(bin_data, aes(x = x,
                     y = y,
                     color = label)) +
  geom_point()+
  labs(title = "Binary Data Distribution",
       xlab = 'x',
       ylab = 'y')


ran <- sample(1:nrow(bin_data), 0.9 * nrow(bin_data)) 

bin_data$x <- scale(bin_data$x)
bin_data$y <- scale(bin_data$y)

bin_train <- bin_data[ran,]
bin_test <- bin_data[-ran,]

bin_target_category <- bin_data[ran,1]
bin_test_category <- bin_data[-ran,1]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

clusters <- numeric(25)
acc <- numeric(25)

for (k in (1:25)) {
  clusters[k] <-  k
  pr <- knn(bin_train,bin_test,cl=bin_target_category,k=k)
  tab <- table(pr,bin_test_category)
  acc[k] <-  accuracy(tab)
}

acc_data <- as.data.frame(bind_cols(clusters, acc))
names(acc_data) <- c("clusters", "accuracy")

acc_data <- acc_data %>% 
  filter(clusters == 3 | clusters == 5 | clusters == 10 | clusters == 15 | clusters == 20 | clusters == 25)

ggplot(acc_data, aes(x = clusters,
                     y = accuracy)) +
  geom_point() +
  labs(title = "Number of clusters vs. accuracy",
       subtitle = "Using k nearest neighbors and binary data",
       xlab = "Clusters",
       ylab = "Accuracy")





trin_data <- read.csv("C:/Users/misherman/DS-520/dsc520/data/trinary-classifier-data.csv")
trin_data$label <- as.factor(trin_data$label)

ggplot(trin_data, aes(x = x,
                     y = y,
                     color = label)) +
  geom_point()+
  labs(title = "Trinary Data Distribution",
       xlab = 'x',
       ylab = 'y')


ran <- sample(1:nrow(trin_data), 0.9 * nrow(trin_data)) 

trin_data$x <- scale(trin_data$x)
trin_data$y <- scale(trin_data$y)

trin_train <- trin_data[ran,]
trin_test <- trin_data[-ran,]

trin_target_category <- trin_data[ran,1]
trin_test_category <- trin_data[-ran,1]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

trin_clusters <- numeric(25)
trin_acc <- numeric(25)

for (k in (1:25)) {
  trin_clusters[k] <-  k
  pr <- knn(trin_train,trin_test,cl=trin_target_category,k=k)
  tab <- table(pr,trin_test_category)
  trin_acc[k] <-  accuracy(tab)
}

acc_trin <- as.data.frame(bind_cols(trin_clusters, trin_acc))
names(acc_trin) <- c("clusters", "accuracy")

acc_trin <- acc_trin %>% 
  filter(clusters == 3 | clusters == 5 | clusters == 10 | clusters == 15 | clusters == 20 | clusters == 25)

ggplot(acc_trin, aes(x = clusters,
                     y = accuracy)) +
  geom_point() +
  labs(title = "Number of clusters vs. accuracy",
       subtitle = "Using k nearest neighbors and trinary data",
       xlab = "Clusters",
       ylab = "Accuracy")


# kmeans on the clustering data
clust_data <- read.csv("C:/Users/misherman/DS-520/dsc520/data/clustering-data.csv")

ggplot(clust_data, aes(x = x,
                      y = y)) +
  geom_point()+
  labs(title = "Cluster Data Distribution",
       xlab = 'x',
       ylab = 'y')

clust_scaled <- scale(clust_data)

#visualize elbow
fviz_nbclust(clust_scaled, kmeans, method = "wss") 

km.res <- vector(mode = "list", length = 11)
clusters <- numeric(length = 11)
avg_SS <- numeric(length = 11)
# Compute k-means with k = 4
for (k in 2:12) {
  set.seed(123)
  clusters[k] <- k
  km.res[[k]] <- kmeans(clust_scaled, k, nstart = 25)
  avg_SS[k] <- mean(km.res[[k]]$withinss)
  
}

clusters <- clusters[-1]
avg_SS <- avg_SS[-1]
kmeans.results <- as.data.frame(bind_cols(clusters, avg_SS))
names(kmeans.results) <- c("clusters", "avg_SS")

ggplot(kmeans.results, aes(x = clusters,
                       y = avg_SS)) +
  geom_point()+
  labs(title = "K means Results",
       x = 'Number of Clusters',
       y = 'Average sum of squares')
