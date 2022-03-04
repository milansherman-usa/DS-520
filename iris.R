library(tidyverse)
library(class)
library(factoextra)

# bin_data <- read.csv("C:/Users/misherman/DS-520/dsc520/data/binary-classifier-data.csv")
# bin_data$label <- as.factor(bin_data$label)

iris_data <- iris %>% 
  select(-Sepal.Length, -Sepal.Width)

ggplot(iris_data, aes(x = Petal.Width,
                     y = Petal.Length,
                     color = Species)) +
  geom_point()+
  labs(title = "Binary Data Distribution",
       xlab = 'x',
       ylab = 'y')

ran <- sample(1:nrow(iris_data), 0.9 * nrow(iris_data)) 

iris_data$Petal.Width <- scale(iris_data$Petal.Width)
iris_data$Petal.Length <- scale(iris_data$Petal.Length)

iris_train <- iris_data[ran,]
iris_test <- iris_data[-ran,]

iris_target_category <- iris_data[ran,3]
iris_test_category <- iris_data[-ran,3]

pr <- knn(iris_train,iris_test,cl=iris_target_category,k=5)
tab <- table(pr,iris_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
acc[k] <-  accuracy(tab)

clusters <- numeric(25)
acc <- numeric(25)

for (k in (1:25)) {
  clusters[k] <-  k
  pr <- knn(iris_train,iris_test,cl=iris_target_category,k=k)
  tab <- table(pr,iris_test_category)
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  acc[k] <-  accuracy(tab)
}

acc_data <- as.data.frame(bind_cols(clusters, acc))
names(acc_data) <- c("clusters", "accuracy")

acc_data <- acc_data %>% 
  filter(clusters == 3 | clusters == 5 | clusters == 10 | clusters == 15 | clusters == 20 | clusters == 25)
```

```{r}
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=20)
tab <- table(pr,iris_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
acc <-  accuracy(tab)

ggplot(acc_data, aes(x = clusters,
                     y = accuracy)) +
  geom_point() +
  labs(title = "Number of clusters vs. accuracy",
       subtitle = "Using k nearest neighbors and binary data",
       xlab = "Clusters",
       ylab = "Accuracy")