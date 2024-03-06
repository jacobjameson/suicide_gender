################################################################################
# AUTHOR: J. Jameson
# LAST MODIFIED: 2024-03-06
#
################################################################################

# Load libraries
library(tidyverse)

# Load data
suicide <- read_csv("raw/suicide_reports.csv")
scores <-  read_csv("raw/2023_scorecard.csv")





table(data$Deaths)

data$Deaths <- gsub("\\*\\*", "", data$Deaths)
data$Deaths <- gsub("\\,", "", data$Deaths)
data$Deaths <- ifelse(data$Deaths == '--', '0', data$Deaths)
data$Deaths <- as.numeric(data$Deaths)

data$Age <- as.numeric(data$Age)
data[is.na(data$Age) == F, ]


data$YearGroup <- cut(data$Year,
                    breaks = c(2000, 2014, 2021),
                    labels = c("2001-2014", "2015-2021"),
                    include.lowest = TRUE)

# Step 2: Calculate average deaths per 100,000 for each age and year group
avg_deaths <- data %>%
  group_by(Age, YearGroup, Sex) %>%
  summarise(AvgDeathsPer100k = sum(Deaths) / sum(Population) * 100000,
            .groups = 'drop')


# Step 3: Plot using ggplot2
avg_deaths %>%
  mutate(group = paste(Sex, YearGroup)) %>%
ggplot( aes(x = Age, y = AvgDeathsPer100k, 
           group = group, 
           color = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000",
       color = "Year Group") +
  # add theme_minimal
  theme_minimal()
