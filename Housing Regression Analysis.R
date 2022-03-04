library(dplyr)
library(tidyverse)
library(janitor)
library(lmtest)
library(regclass)

options(scipen = 999)

housing <- readxl::read_excel("C:/Users/misherman/DS-520/dsc520/data/week-7-housing.xlsx") %>% 
  clean_names()


# I am creating a single variable for the number of bathrooms and extracting the year of sale from date sold
# I also make year_sold numeric and zip code a factor for the regression model 

# I also exclude the 98059 zip code as I believe that zip code will be correlated with sales price
# and this zip code only has one sale and thus introduces noise into the model

housing <- housing %>% 
  dplyr::filter(zip5 != '98059') %>% 
  mutate(bathrooms = bath_full_count + 0.5*bath_half_count + .75*bath_3qtr_count, 
         year_sold = sale_date)

housing$year_sold <- format(housing$year_sold, format = '%Y')
housing$year_sold <- as.numeric(housing$year_sold)

housing <- housing %>% 
  mutate(home_age = year_sold - year_built)

# two-variable model
lot_lm <- lm(sale_price ~ sq_ft_lot, data = housing)
               

# multiple regression model
all_lm <- lm(sale_price ~ sq_ft_lot + 
               square_feet_total_living + 
               bedrooms +
               bathrooms +
               year_sold + 
               home_age +
               zip5, 
             data = housing)

# in addition to the size of the lot, the amount of living space will also affect the price
# in addition to the amount of living space, the number of bedrooms and bathrooms should also affect the price
# home prices tend to change over time, so knowing when the home was sold should help to predict its price
# how old or new the home is will also affect the price, which is why I created home_age
# the three most important factors in real estate are location, location, and location, so I'm including the
# the zip code as a predictor of price.  Although all these homes are in the same area, I suspect some zip
# codes are more desirable to live in than others.  Note that above I removed the zip code 98059 from the
# data in the model as only one home was sold in this zip code and thus could add noise to our model

summary(lot_lm)

summary(all_lm)
# the R squared for the two variable model is 0.01435 and the adjusted R squared is 0.01428
# the R squared for the multiple predictor model is 0.2261 and the adjusted R squared is 0.2257
# this indicates that the model with multiple predictors explains more than 20% more of the variance 
# in sale price than lot size alone

# explanation of the beta values
# the intercept doesn't have much meaning in this context as it gives the sale price for a home given that
# all the predictors are 0 in zip code 98052, i.e. a home with 0 living space and 0 lot size
# lot size
# the lot size coefficient of 0.39 means that for every increase of 1 square foot in the lot size, the price
# of the home increase by 39 cents.  While this doesn't seem like much, many of the lots are quite large
# living space
# the living space coefficient is about 160 meaning that for every additional square foot of living space,
# the price of the home increases by $160
# bedrooms
# counter intuitively (at least to me), the number of bedrooms is negatively correlated with the sale price, i.e.
# for each additional bedroom, the price of the home decreases by over $17k.  I suspect that this means that there
# are some non-residential properties in the data that sell for a lot of money
# bathrooms
# the number of bathrooms is also negatively correlated with the sale price, with the sale price decreasing by about
# $2000 for each addtional bathroom.  This further supports my hypothesis that this dataset contains non-residential
# properties
# sale year
# the sale year coefficient is 5370, which means that each year the price of a home increases by about $5370 on average
# home age
# the home age coefficient is about -3637, meaning that for each year older a home is the price decreases
# by $3637
# zip codes
# as the zip code variable is categorical, we interpret the coefficients relative to the intercept
# there are three unique zip codes (after removing one that had only one sale): 98052, 98053, and 98074
# as 98074 and 98053 are listed in the output, we can infer that zip code 98052 is part of the default model, i.e.
# the intercept.  Thus, we interpret the beta coefficients for zip code 98053 and 98074 relative to 98052
# zip code 98053
# this zip code is negatively correlated with sale price, with homes being sold in this zip code selling for 
# $42k less than in zip code 98052, on average
# zip code 98074
# this zip code is positively correlated with sale price, with homes being sold in this zip code selling for 
# $67k more than in zip code 98052, on average
# NOTE: all predictors are significant in the model with the exception of bathrooms and 98074 zip code


# confidence intervals

I
# (Intercept)              -21151679.5857112 -13342871.6711227
# sq_ft_lot                        0.2390317         0.4788744
# square_feet_total_living       163.3683428       184.7412236
# bedrooms                    -26983.8246615     -7731.5620840
# bathrooms                   -16407.4444415     12387.0190647
# year_sold                     6807.5534700     10695.4173912
# home_age                     -3712.5527573     -2834.9704295
# zip598053                   -57221.7345318    -28231.5521194
# zip598074                   -15501.1197207    149404.0218346

# in general, the smaller the confidence interval the better (relative to the estimate).   
# so, for example, the interval for sq_ft_lot is very small, but so is the estimate (0.39)
# in general, if the confidence interval crosses 0, that is not a good predictor, which is the case
# with bathrooms and zip code 98074

anova(lot_lm, all_lm)

# the ANOVA has an F value of over 500, and thus the multiple regression model is significantly better than
# the two variable model

# in addition to comparing the two-variable and multiple regression model, I am going to create another multiple
# regression model without zip code to see if the inclusion of zip code makes the model better or not, given that
# not all levels of zip code are significant to the model
# in addition, I will remove bathrooms from the model as it is not a significant predictor

no_zip_lm <-  lm(sale_price ~ sq_ft_lot + 
                   square_feet_total_living + 
                   bedrooms +
                   year_sold +
                   home_age,
                 data = housing)

summary(no_zip_lm)
anova(no_zip_lm, all_lm)
# it appears that the inclusion of zip code does not significantly improve the model, so I will leave it out

# we now add diagnostic tests to check the accuracy of the model
housing <- housing %>% 
  mutate(resid = resid(no_zip_lm),
         stz.r = rstandard(no_zip_lm),
         stu.r = rstudent(no_zip_lm),
         cooks = cooks.distance(no_zip_lm),
         dfbeta = dfbeta(no_zip_lm),
         diffit = dffits(no_zip_lm),
         leverage = hatvalues(no_zip_lm),
         cov.ratios = covratio(no_zip_lm))

# to identify the cases which have large residuals, I subset the above dataframe by filtering on the standardized
# residual field
res.lg <- housing %>% 
  select(sale_price,
         sq_ft_lot, 
         square_feet_total_living, 
         bedrooms,
         year_sold,
         home_age, 
         stz.r) %>% 
  filter(stz.r > 2 | stz.r < -2) %>% 
  mutate(abs_stz = abs(stz.r))

# there are a total of 333 cases (2.6%) with residuals greater than 2 (in absolute value), 
# which is less than 5% of the sample
# however, 242 cases have a residual greater than 2.58, which is 1.9%
# there are 199 cases where the standardized residuals are greater than 3.3 (in absolute value)


lev_limit <- 3*6/nrow(housing)
housing %>% filter(leverage > lev_limit) %>% summarise(n())
# there are 364 cases with a leverage value above the recommended limit of 3 times the average 

cov_ratio_low <- 1 - lev_limit 
cov_ratio_high <- 1 + lev_limit 
housing %>% filter(cov.ratios > cov_ratio_high | cov.ratios < cov_ratio_low) %>% summarise(n())
# there are 691 cases where the covariance ratio is outside of the recommended limits


# testing the assumption of independence of residuals
dwtest(no_zip_lm)

# Durbin-Watson test
# data:  no_zip_lm
# DW = 0.55963, p-value < 0.00000000000000022
# alternative hypothesis: true autocorrelation is greater than 0

# it appears that there is a very low probability that the residuals are uncorrelated

# testing the assumption of no multicollinearity
vif <- VIF(no_zip_lm)
# sq_ft_lot square_feet_total_living                 bedrooms                year_sold                 home_age 
# 1.144655                 1.934761                 1.610110                 1.054216                 1.287139 

1/vif
#                sq_ft_lot      square_feet_total_living            bedrooms                year_sold                 home_age 
#                 0.8736254                0.5168598                0.6210756                0.9485726                0.7769170 

mean(vif)
#  1.406176

# there are no predictors with a VIF greater than 2 or tolerance less than 0.5, and the mean VIF is 1.4, all suggesting that the predictors have a lowe level 
#  multicollinearity


plot(no_zip_lm)

hist(housing$stu.r)

# the plot of fitted values vs. residuals dispalys a fanning out pattern that suggests heteroscedasticity , i.e. a  violation of the assumption of 
# homogeneity of variance
# furthermore, the Q-Q plot and the historgram of the studentized residuals indicate that the residuals are not normally distributed, but positively skewed

# due to the violation of the assumptions of homogeneity of variance, the independence of residuals, and residuals being normally distributed, it seems that this
# is not unbiased, i.e., we would have little confidence using it to make out of sample predictions