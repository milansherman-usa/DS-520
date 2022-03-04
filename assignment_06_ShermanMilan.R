# Assignment: ASSIGNMENT 6
# Name: Sherman, Milan
# Date: 2022-01-22

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/misherman/DS-520/dsc520")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

## Load the ggplot2 library
library(tidyverse)

## Fit a linear model using the `age` variable as the predictor and `earn` as the outcome
age_lm <-  lm(earn ~ age,  heights_df)

## View the summary of your model using `summary()`
summary(age_lm)

## Creating predictions using `predict()`
age_predict_df <- data.frame(earn_pred = predict(age_lm, heights_df), age=heights_df$age)

## Plot the predictions against the original data
ggplot(data = heights_df, aes(y = earn, x = age)) +
  geom_point(color='blue') +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color = 'red')


mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - age_predict_df$earn)^2)
## Residuals
residuals <- heights_df$earn - age_predict_df$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared R^2 = SSM\SST
r_squared <- ssm/sst

## doing it another way
# join heights df with preds df
# create columns for squared deviations, squared predicted deviations, and squared residuals
# create summary measures (sst, ssm, sse, r squared)

summary <- heights_df %>% 
  select(age, earn) %>% 
  mutate(ID = 1,
         UID = cumsum(ID)) %>% 
  inner_join(age_predict_df %>% 
               mutate(ID = 1,
                      UID = cumsum(ID)), by = "UID") %>% 
  mutate(dev_sqd = (earn - mean_earn)^2,
         dev_mod = (earn_pred - mean_earn)^2,
         res_sqd = (earn - earn_pred)^2) %>% 
  summarise(sst = sum(dev_sqd),
            ssm = sum(dev_mod),
            sse = sum(res_sqd),
            r_squared = ssm/sst)


## Number of observations
n <- nrow(heights_df)
## Number of regression parameters
p <- 2
## Corrected Degrees of Freedom for Model (p-1)
dfm <- p-1
## Degrees of Freedom for Error (n-p)
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse/dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst/dft
## F Statistic F = MSM/MSE
f_score <- msm/mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1-(1 - r_squared)*dft/dfe

## Calculate the p-value from the F distribution
p_value <- pf(f_score, dfm, dft, lower.tail=F)

summary(age_lm)
