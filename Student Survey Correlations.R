library(tidyverse)

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/misherman/DS-520/dsc520")

## Load the data
student_survey <- read.csv("data/student-survey.csv")
cov(student_survey)
cor(student_survey)


# Data vectors
x <- rnorm(2)
y <- rnorm(2)

# Binding into square matrix
mat <- cbind(x, y)
print(mat)

# Defining X as the covariance matrix
X <- cov(mat)

# Print covariance matrix
print(X)

# Print correlation matrix of data
# vector
print(cor(mat))

# Using function cov2cor()
# To convert covariance matrix to
# correlation matrix
print(cov2cor(X))
