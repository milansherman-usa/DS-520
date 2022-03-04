# Assignment: ASSIGNMENT 7
# Name: Sherman, Milan
# Date: 2022-01-22

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/misherman/DS-520/dsc520")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

# Fit a linear model
earn_lm <-  lm(earn ~ age + sex + ed + height + race, data=heights_df)

# View the summary of your model
summary(earn_lm)

predicted_df <-heights_df %>% 
  mutate(earn_pred= predict(earn_lm, heights_df))

## Compute deviation (i.e. residuals)
mean_earn <- mean(heights_df$earn)

summary <- predicted_df %>% 
  mutate(dev_sqd = (earn - mean_earn)^2,
         dev_mod = (earn_pred - mean_earn)^2,
         res_sqd = (earn - earn_pred)^2) %>% 
  summarise(sst = sum(dev_sqd),
            ssm = sum(dev_mod),
            sse = sum(res_sqd),
            r_squared = ssm/sst)

## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum(mean_earn - predicted_df$earn_pred)
## Residuals
residuals <- predicted_df$earn - predicted_df$earn_pred
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared
r_squared <- SSM/sst

## Number of observations
n <- nromw(predicted_df)
## Number of regression paramaters
p <- 5
## Corrected Degrees of Freedom for Model
dfm <- p-1
## Degrees of Freedom for Error
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse/dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst/dft
## F Statistic
f_score <- msm/mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1-(1 - r_squared)*dft/dfe
