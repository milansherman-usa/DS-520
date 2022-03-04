library(dplyr)
library(tidyverse)
scores <- read.csv("C:/Users/misherman/DS-520/dsc520/data/scores.csv")
summary(scores)
str(scores)

sports <- scores %>% 
  filter(Section == 'Sports')

regular <- scores %>% 
  filter(Section == 'Regular')

section_size <- scores %>% 
  group_by(Section) %>% 
  summarise(size = sum(Count))

ggplot(sports, aes(x = Score,
                   y = Count)) +
  geom_col() +
  labs(title = "Count of Test Scores",
       subtitle = "Sports Themed Section",
             x = "Test Score",
             y = "Count")

ggplot(regular, aes(x = Score,
                   y = Count)) +
  geom_col() +
  labs(title = "Count of Test Scores",
       subtitle = "Regular Section",
       x = "Test Score",
       y = "Count")

ggplot(scores, aes(x = Score,
                    y = Count, 
                   fill = Section)) +
  geom_col(position = "dodge") +
  labs(title = "Count of Test Scores",
       x = "Test Score",
       y = "Count") +
  theme(legend.position = "top")

# looking at the plot of both sections on the same graph, it appears that in general the Regular section
# scored higher than the sports section, which can be seen by seeing more of the Regular section on the 
# right of the plot and more of the Sports section on the left


# However, the highest and lowest scores were in the Sports section, so there is not one section that has
# all scores higher or lower than the other.  Another confounding factor is that the sections are not the same
# size, i.e, there are more students in the Regular section

# in terms of tendency and consistency, from the graph it appears that the Sports section tends to have lower 
# scores and more variability, while the Regular section tends to have higher and tighter distribution

# An important missing variable that would help to interpret these results is the prior achievement of these
# students.  It could be the case the students who chose the Regular section were better prepared for the 
# course than those who chose the Sports themed section, and that the scores are measuring that difference
# and not the effect of the context of the examples used in each section



