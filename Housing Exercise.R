library(dplyr)
library(tidyverse)
library(janitor)

options(scipen = 999)

housing <- readxl::read_excel("C:/Users/misherman/DS-520/dsc520/data/week-7-housing.xlsx") %>% 
  clean_names()

housing$zip5 <-   as.factor(housing$zip5)

# I noticed that the 98059 zip code only has one sale, so I am removing it from the analysis
# of sales metrics by zip code
# I am also creating a variable for price per square foot

housing <- housing %>% 
  mutate(ppsf = sale_price/square_feet_total_living)

zip_summary <- housing %>% 
  dplyr::group_by(zip5) %>% 
  dplyr::filter(zip5 != '98059') %>% 
  summarise(sales = n(),
            avg_price = mean(sale_price),
            avg_size = mean(square_feet_total_living),
            avg_ppsf = mean(ppsf))

# summarising these metrics by zip code, we see that while the 98074 zip area has the highest average
# price of the three areas, it also has the largest homes, and thus ends up with the smallest
# price per square foot of the three areas


# create a field for the year of date sold
housing <- housing %>% 
  mutate(year = sale_date)
housing$year <- format(housing$year, format = '%Y')


# find the average price of a house sold in a given year as a new field in the housing dataframe
housing <- housing %>% 
  left_join(housing %>% 
              group_by(year) %>% 
              summarise(avg_price = mean(sale_price)),
            by = "year")

# plot home prices by year
ggplot(housing, aes(x = year, 
                    y = avg_price,
                    group = 1)) +
  geom_line() +
  labs(title = "Home Prices by Year",
       x = "Year",
       y = "Average Price")

# this plot clearly shows the bursting of the housing bubble in 2008, with prices being
# highest in 2008 and lowest in 2009 during this 10 year period


# add average price by zip code by year
housing <- housing %>% 
  left_join(housing %>% 
              filter(zip5 != '98059') %>% 
              group_by(zip5, year) %>% 
              summarise(price_by_zip_yr = mean(sale_price)),
            by = c("zip5", "year"))

ggplot(housing %>% 
         filter(zip5 != '98059'), aes(x = sale_date,
                    y = price_by_zip_yr,
                    color = zip5)) +
  geom_line() +
  theme(legend.position = "top") +
  labs(title = "Home Prices by Year",
       subtitle = "By Zip Code",
       color = "Zip Code",
       x = "Year of Sale",
       y = "Average Price")


library(purrr)
# keep all the homes built after 2000
keep(housing$year_built, function(x) x > 2000)

# keep all the homes that have been renovated
keep(housing$year_renovated, function(x) x !=0)

# discard all the homes where the city is NA
discard(housing$ctyname, is.na)

# discard all the homes where there is no sale warning
# i.e., keep all the homes where there is a sale warning
discard(housing$sale_warning, is.na)

# use cbind to extract sales price and home size
# create a scatterplot to visualize the relationship, which generally looks positive
price_size <- as.data.frame(cbind(housing$sale_price, housing$square_feet_total_living))
names(price_size) <- c("sale_price", "home_size")
ggplot(price_size, aes(x = home_size,
                       y = sale_price)) +
  geom_point()

# use rbind to create a dataframe of home built after 2000 OR with more than 4000 sq ft of living space
big_new <- rbind(housing %>% filter(year_built > 2000), housing %>% filter(square_feet_total_living > 4000))

# use string split to separate addresses into number and street parts
address <- housing$addr_full
address2 <- str_split(address, " ", n = 2)

# create a vector of streets and determine how many are unique
street <- character()
for (i in 1:length(address2)) {
  street[i] <- address2[[i]][2]
}
street_distinct <- unique(street)

# we see that these 12.8 thousand homes exist on 730 unique streets (as long as different streets don't have the same name)

# create a separate vector of house numbers
number <- character()
for (i in 1:length(address2)) {
  number[i] <- address2[[i]][1]
}

# concatenate the numbers and streets back together into a street address using paste
street_address <- paste(number, street)
street_address
