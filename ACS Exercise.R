setwd("C:/Users/misherman/DS-520/dsc520/data")
ACS <- read.csv("acs-14-1yr-s0201.csv")

# elements of the dataframe
# ID (character)
# ID2 which appears to be the last four digits of ID (integer)
# Geography (character)
# PopGroupID (integer)
# POPGROUP.display.label (character)
# RacesReported (integer)
# HSDegree (numeric)
# BachDegree (numeric)


str(ACS)

# 'data.frame':	136 obs. of  8 variables:
# $ Id                    : chr  "0500000US01073" "0500000US04013" "0500000US04019" "0500000US06001" ...
# $ Id2                   : int  1073 4013 4019 6001 6013 6019 6029 6037 6059 6065 ...
# $ Geography             : chr  "Jefferson County, Alabama" "Maricopa County, Arizona" "Pima County, Arizona" "Alameda County, California" ...
# $ PopGroupID            : int  1 1 1 1 1 1 1 1 1 1 ...
# $ POPGROUP.display.label: chr  "Total population" "Total population" "Total population" "Total population" ...
# $ RacesReported         : int  660793 4087191 1004516 1610921 1111339 965974 874589 10116705 3145515 2329271 ...
# $ HSDegree              : num  89.1 86.8 88 86.9 88.8 73.6 74.5 77.5 84.6 80.6 ...
# $ BachDegree            : num  30.5 30.2 30.8 42.8 39.7 19.7 15.4 30.3 38 20.7 ...

nrow(ACS)
# [1] 136
ncol(ACS)
# [1] 8


p <- ggplot(ACS, aes(x = HSDegree)) +
  geom_histogram(aes(y = ..density..), bins = 25) +
  labs(title = "High School Degrees by Location",
       x = "Percent of residents with a high school degree",
       y = "Frequency")

# the distribution is unimodeal as can be seen from a single "hump" in the histogram
# the distribution is not symmetic, bell-shaped, and does not appear to be approximately normal
# this can be seen in the histogram with a majority of values bunched on the right side of the graph
# and a few values further away from these to the left
# this shape appears to be negatively skewed, or skewed left

p + stat_function(fun = dnorm, args = list(mean = mean(ACS$HSDegree, na.rm = TRUE),
                                           sd = sd(ACS$HSDegree, na.rm = TRUE)),
                  colour = "black", 
                  size = 1)

# after adding the normal distribution curve, it appears that this distribution is not normal
# there is a large gap under the normal curve on the left side, and a large part of the distribution
# above the normal curve on the right side
# this further confrims the hypothesis that this distribution is negatively skewed

ggplot(ACS, aes(sample = HSDegree)) +
  geom_qq() +
  geom_qq_line()

# as the graph is not approximately a straight line, the distribution is not normally distributed
# as the graph bends downward (concave) this confirms that the distribution is negatively skewed
# in particular the data points on the left side of the graph are less than what would be predicted by a normal distribution,
# confirming a negative skew



library(pastecs)

stat.desc(ACS$HSDegree, basic = FALSE, norm = TRUE)

# The skew and kurtosis of this distribution seem to be highly improbable for a normal distribution.
# This can be seen from the skew.2SE and kurt.2SE values (the skew and kurtosis z-scores), which are not close to 1
# However, these values may be inflated by the size of the sample, as the standard error is fairly small due to the sample size
# That is, it's possible that with a smaller sample, and therefore a larger standard error, the skew and kurtosis z-scores
# would be closer to or less than 1