library(plyr)
library(janitor)

options(scipen = 999)

housing <- readxl::read_excel("C:/Users/misherman/DS-520/dsc520/data/week-7-housing.xlsx") %>% 
  clean_names()

# use apply to change the city and postal county to lowercase
housing$ctyname <- lapply(housing$ctyname, tolower)
housing$postalctyn <- lapply(housing$postalctyn, tolower)

#use aggregate to find the average sale price of homes by zip code
aggregate(sale_price ~ zip5, housing, mean)

#use ddply to find the average home price by sale reason
ddply(housing, "sale_reason", summarise, mean.count = mean(sale_price))

# check distribution of quantitative variables (sale price, living space, lot size)
# generate histograms of each

library(ggplot2)
ggplot(housing, aes(x = sale_price)) +
  geom_histogram() +
  labs(title = "Distribution of Home Sale Prices",
       x = "Sale Price",
       y = "Frequency")

ggplot(housing, aes(x = sq_ft_lot)) +
  geom_histogram()+
  labs(title = "Distribution of Lot Size",
       x = "Lot Size (square feet)",
       y = "Frequency")


ggplot(housing, aes(x = square_feet_total_living)) +
  geom_histogram()+
  labs(title = "Distribution of Home Size",
       x = "Living Space (square feet)",
       y = "Frequency")

# the histograms show that each of these variables are positively skewed

# use qq plot to check for normality visually

ggplot(housing, aes(sample = sale_price)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Sale Price")

ggplot(housing, aes(sample = sq_ft_lot)) +
  geom_qq() +
  geom_qq_line()+
  labs(title = "Lot Size")

ggplot(housing, aes(sample = square_feet_total_living))+
  geom_qq() +
  geom_qq_line()+
  labs(title = "Home Size")

# none of the variables appear to be normally distributed, especially at the upper end

# I create a function to determine if a distribution has an outlier using the IQR definition of outlier

outlier_check <- function(var) {
  summary <- fivenum(var)
  q1 <- summary[2]
  q3 <- summary[4]
  iqr <- q3 - q1
  lower <- q1 - 2.5*iqr
  upper <- q3 + 2.5*iqr
  
  if (summary[1] < lower){
    print("Low outlier")}
  if (summary[5] > upper){
    print("High outlier")}
  if (summary[1] >= lower & summary[5] <= upper){
    print("No outliers")}
}
    
outlier_check(housing$sale_price)
outlier_check(housing$square_feet_total_living)
outlier_check(housing$sq_ft_lot)

# consistent with the positive skew, each of these variables have outliers on the upper end

# create new variables for mean and median sales price by sales reason
housing <- ddply(housing, "sale_reason", mutate, mean.count = mean(sale_price), median.count = median(sale_price))
