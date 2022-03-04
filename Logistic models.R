library(foreign)
library(tidyverse)
library(aod)
library(janitor)

# Thoracic Survey dataset

df <- read.arff("C:/Users/misherman/DS-520/dsc520/data/ThoraricSurgery.arff")

# I tried different variables in my model and found these to be significant

mylogit <- glm(Risk1Yr ~ PRE14 + PRE30 + PRE9 + PRE17 + PRE25, data = df, family = "binomial")

summary(mylogit)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 1:4)

exp(coef(mylogit))


df$rankP <- predict(mylogit, newdata = df, type = "response")

df <- df  %>% mutate(model_pred = 1*(rankP > .50) + 0,
                     survive_binary = 1*(Risk1Yr == "T") + 0,
                     accurate = 1*(model_pred == survive_binary))

df %>% summarise(accuracy = sum(accurate)/nrow(df))


df %>% tabyl(Risk1Yr)


bin_data <- read.csv("C:/Users/misherman/DS-520/dsc520/data/binary-classifier-data.csv")

mylogit2 <- glm(label ~ x + y, data = bin_data, family = "binomial")

summary(mylogit2)


exp(coef(mylogit2))


bin_data$rankP <- predict(mylogit2, newdata = bin_data, type = "response")

bin_data <- bin_data  %>% mutate(model_pred = 1*(rankP > .50) + 0,
                     accurate = 1*(model_pred == label))

bin_data %>% summarise(accuracy = sum(accurate)/nrow(bin_data))


bin_data %>% tabyl(label)
