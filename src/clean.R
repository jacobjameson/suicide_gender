################################################################################
# AUTHOR: J. Jameson
# LAST MODIFIED: 2024-03-06
#
################################################################################

# Load libraries
library(tidyverse)

################################################################################
# Prepare scorecard data
scores <-  read_csv("raw/2023_scorecard.csv")

scores <- scores %>%
  mutate(grade = case_when(
    Grade == 'A' ~ "High Grade",
    Grade == 'A-' ~ "High Grade",
    Grade == 'B+' ~ "High Grade",
    Grade == 'B' ~ "High Grade",
    Grade == 'B-' ~ "High Grade",
    Grade == 'C+' ~ "High Grade",
    TRUE ~ 'Low Grade')
    )
################################################################################
# Prepare suicide data
suicide <- read_csv("raw/suicide_reports.csv")

suicide$Deaths <- gsub("\\*\\*", "", suicide$Deaths)
suicide$Deaths <- gsub("\\,", "", suicide$Deaths)
suicide$Deaths <- ifelse(suicide$Deaths == '--', '10', suicide$Deaths)
suicide$Deaths <- as.numeric(suicide$Deaths)

suicide$Age <- as.numeric(suicide$Age)
suicide[is.na(suicide$Age) == F, ]

suicide <- suicide %>%
  filter(State != 'Rhode Island', 
         State != 'District of Columbia')

suicide <- merge(suicide, scores, on='State')

suicide$YearGroup <- cut(suicide$Year,
                    breaks = c(2000, 2014, 2021),
                    labels = c("2001-2014", "2015-2021"),
                    include.lowest = TRUE)

# Step 2: Calculate average deaths per 100,000 for each age and year group
avg_deaths <- suicide %>%
  group_by(Age, grade) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop')


library(ggsci)


avg_deaths %>%
  mutate(group = paste(Sex, grade)) %>%
  filter(Sex != 'NA') %>%
ggplot( aes(x = Age, y = AvgDeathsPer100k, 
           group = group, 
           color = group)) +
  geom_vline(xintercept = 65, linetype = 'dashed') + 
  geom_vline(xintercept = 21, linetype = 'dashed') + 
  geom_smooth(method = "loess", se = TRUE, size = 0.8, co) + # Added line for smoothing
  scale_color_jama() +
  xlim(c(10,80)) +
  labs(title = "Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000",
       color = "Year Group") +
  theme_minimal()


# create a graph of avg_deaths thats smoothes

avg_deaths %>%
  ggplot(aes(x = Age, y = AvgDeathsPer100k, 
              color = grade)) +
  geom_point() + geom_line() +
  scale_color_jama() +
  labs(title = "Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000",
       color = "Year Group") +
  theme_minimal()

# Step 3: Calculate average deaths per 100,000 for each age and year group
library(ggplot2)

# Assuming you have a rate or similar metric calculated
ggplot(avg_deaths, aes(x = Age, y = AvgDeathsPer100k, color = grade)) +
  geom_point(alpha = 0.6) +  # Adjust opacity as necessary
  geom_smooth(method = "loess", se = FALSE) +  # Loess smoothing
  labs(title = "Firearm Suicide Rates by Age-Year",
       x = "Age",
       y = "Rate (Per 100K or your metric)",
       color = "Policy Grade") +
  theme_minimal()


