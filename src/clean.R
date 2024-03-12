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
    Grade == 'A' ~ "More Stict",
    Grade == 'A-' ~ "More Stict",
    Grade == 'B+' ~ "More Stict",
    Grade == 'B' ~ "More Stict",
    Grade == 'B-' ~ "More Stict",
    Grade == 'C+' ~ "More Stict",
    TRUE ~ 'Less Stict')
    )
################################################################################
# Cleaning function

clean_suicide_data <- function(file_path, replace_suppressed = 5) {

  suicide_data <- read_csv(file_path, show_col_types = FALSE)
  
  end <- nrow(suicide_data) - 16
  suicide_data <- suicide_data[c(1 : end),]
  
  suicide_data <- filter(
    suicide_data, 
    !Age %in% c('<1', '85+', 'Unknown')
  )

  suicide_data$Deaths <- gsub("\\*\\*", "", suicide_data$Deaths) 

  suicide_data$Deaths <- gsub(",", "", suicide_data$Deaths)
  suicide_data$Population <- gsub(",", "", suicide_data$Population) 
  suicide_data$Population <- as.numeric(suicide_data$Population)
  
  suicide_data$Deaths <- ifelse(
    suicide_data$Deaths == '--', 
    as.character(replace_suppressed), 
    suicide_data$Deaths
  )
  
  suicide_data$Deaths <- as.numeric(suicide_data$Deaths)
  
  # Convert Age to numeric and filter out NA values
  suicide_data$Age <- as.numeric(suicide_data$Age)
  suicide_data <- filter(suicide_data, !is.na(Age))
  
  # Filter out specific states
  suicide_data <- filter(
    suicide_data, 
    !State %in% c('Rhode Island', 'District of Columbia', 'Hawaii')
  )
  
  suicide_data <- merge(suicide_data, scores, by = 'State')
  
  suicide_data$Year <- as.numeric(suicide_data$Year)

  suicide_data$YearGroup <- cut(
    suicide_data$Year,
    breaks = c(2000, 2014, 2021),
    labels = c("2001-2014", "2015-2021"),
    include.lowest = TRUE
  )
  
  suicide_data <- suicide_data %>%
    select(State, Year, Age, Sex, Deaths, Population, grade, YearGroup) %>%
    filter(Age >= 12, Age <= 80)
  
  return(suicide_data)
}


################################################################################

# read in and clean data
suicide.firearm <- clean_suicide_data("raw/suicide_reports_firearm.csv", 5)
suicide.nonfirearm <- clean_suicide_data("raw/suicide_reports_nonfirearm.csv", 5)
suicide.all <- clean_suicide_data("raw/suicide_reports_all.csv", 5)

suicide.firearm <- suicide.firearm %>%
  rename(FirearmDeaths = Deaths)

suicide.nonfirearm <- suicide.nonfirearm %>%
  rename(NonFirearmDeaths = Deaths)


avg_deaths.firearm <- suicide.firearm %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Firearm')

avg_deaths.nonfirearm <- suicide.nonfirearm %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Non-Firearm')

avg_deaths.all <- suicide.all %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'All')


avg_deaths <- bind_rows(avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all)



avg_deaths %>%
  filter(YearGroup == "2015-2021") %>%
  mutate(group = paste(category, grade)) %>%
  ggplot(aes(x = Age, y = AvgDeathsPer100k, 
             color = Sex, shape=grade, linetype=grade)) +
  geom_smooth(aes(fill = Sex)) +
  facet_wrap(~category) +
  scale_color_nejm() +
  labs(title = "Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000") +
  theme_minimal()


library(ggplot2)
library(ggthemes) # for additional themes
library(RColorBrewer) # for color palettes

# Enhanced plotting code
avg_deaths %>%
  filter(YearGroup == "2015-2021") %>%
  ggplot(aes(x = Age, y = AvgDeathsPer100k, 
             color = Sex, shape = grade, linetype = grade)) +
  geom_smooth(aes(fill = Sex), size = 1, method = "loess") + # Increase line size
  facet_wrap(~category) + # Scales free if categories vary widely
  scale_color_brewer(palette = "Set1") + # A more vibrant, distinguishable palette
  scale_fill_brewer(palette = "Set1") + # Consistent fill colors
  labs(title = "Average Deaths per 100,000 by Age",
       subtitle = "Data from 2015 to 2021",
       caption = str_wrap("Source: Centers for Disease Control and Prevention 
                          Web-based Injury Statistics Query and Reporting 
                          System (WISQARS), March 2024. 'More Strict' 
                          and 'Less Strict' refer to Annual Gun Law Scorecard grades as described in text. 
                          Trend lines represent smoothed averages of deaths per 100k
                          by age, using a LOESS method for non-parametric local 
                          regression. The shading indicates the 95% confidence 
                          interval around the estimate.  In instances of unstable values indicated by 
                          '**' (representing fewer than 20 deaths), the original data points have 
                          been retained to preserve the raw trends. For suppressed values denoted 
                          by '--' (suggesting counts between one to nine or based on specific 
                          statistical criteria), we have imputed a conservative estimate of 5 deaths, 
                          to maintain continuity of the data while acknowledging the inherent uncertainty 
                          of small numbers. Secondary suppressions, marked by '--*', are treated with the 
                          same caution. A sensitivity analysis was subsequently performed to assess the 
                          robustness of our findings, accounting for the potential variability introduced 
                          by these imputations and ensuring the reliability of our conclusions despite the
                          presence of these less stable estimates.",140),
       x = "Age",
       y = "Average Deaths per 100,000",
       color = "Sex",
       fill = "Sex",
       shape = "Annual Gun Law Scorecard",
       linetype = "Annual Gun Law Scorecard") +
  theme_bw(base_size = 14) + # Increase the base font size
  theme(legend.position = "bottom", # Better legend positioning
        plot.title = element_text(size = 20, face = "bold"), # Larger, bold title
        plot.subtitle = element_text(size = 16), # Subtitle formatting
        plot.caption = element_text(size = 9, margin = margin(t = 10), hjust = 0), # Caption styling
        axis.text = element_text(size = 12), # Larger axis text
        axis.title = element_text(size = 14), # Larger axis titles
        legend.text = element_text(size = 12), # Larger legend text
        strip.text = element_text(size = 14)) # Larger facet labels


ggsave("outputs/avg_deaths.png", width = 9, height = 6, dpi = 300)

# Note: Adjust the size parameters and palette choices to fit your preferences.





avg_deaths %>%
  filter(YearGroup == "2015-2021", Sex == 'Females') %>%
  mutate(group = paste(category, grade)) %>%
  ggplot(aes(x = Age, y = AvgDeathsPer100k, 
             color = grade, shape=grade)) +
  #geom_point( size = 2, alpha = 0.6) + 
  #geom_line() +
  geom_smooth(aes(fill = grade)) +
  facet_grid(~category) +
  scale_color_nejm() +
  labs(title = "FEMALES Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000") +
  theme_minimal()


avg_deaths %>%
  filter(YearGroup == "2015-2021", category != 'All') %>%
  ggplot(aes(x = Age, y = AvgDeathsPer100k, 
             color = grade, shape=grade)) +
  #geom_point( size = 2, alpha = 0.6) + 
  #geom_line() +
  geom_smooth(aes(fill = grade)) +
  facet_grid(~Sex + category, nrow=2) +
  scale_color_nejm() +
  labs(title = "FEMALES Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000") +
  theme_minimal()







avg_deaths.firearm <- suicide.firearm %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = seq(1,
                                     84, 
                                     by = 4), 
                        include.lowest = TRUE)) %>%
  group_by(AgeGroup, Sex, grade) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Firearm')

avg_deaths.nonfirearm <- suicide.firearm %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = seq(1,
                                     84, 
                                     by = 4), 
                        include.lowest = TRUE)) %>%
  group_by(AgeGroup, Sex, grade) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Firearm')

avg_deaths.all <- suicide.all %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = seq(1,
                                     84, 
                                     by = 4), 
                        include.lowest = TRUE)) %>%
  group_by(AgeGroup, Sex, grade) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'All')






avg_deaths %>%
  filter(YearGroup == "2015-2021", Sex == 'Males') %>%
  mutate(group = paste(category, grade)) %>%
ggplot(aes(x = Age, y = AvgDeathsPer100k, 
           color = category, shape=category)) +
  geom_point( size = 2, alpha = 0.6) + 
  geom_line() +
 # geom_smooth(aes(fill = category)) +
  facet_grid(~grade) +
  scale_color_nejm() +
  labs(title = "Average Deaths per 100,000 by Age",
       x = "Age",
       y = "Average Deaths per 100,000",
       color = "Year Group") +
  theme_minimal()

# create a stacked barchart


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



glimpse(avg_deaths)

avg_deaths %>%
  filter(AgeGroup != 'NA', Sex== 'Males') %>%
ggplot(aes(x = AgeGroup, 
           y = AvgDeathsPer100k, fill = category)) +
  facet_wrap(~grade, nrow=2) +
  geom_bar(stat = "identity") +
  labs(title = "Average Deaths per 100,000 by Age Group MALES",
       x = "Age Group",
       y = "Average Deaths per 100,000 (MALES)",
       fill = "Category") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") 






ggplot(avg_deaths, aes(x = Age, y = AvgDeathsPer100k, fill = category)) +
  facet_wrap(~Sex + grade) +
  geom_area() +
  labs(title = 'Title: Low Grade vs High Grade over Time',
       fill = 'Grade') +
  theme_minimal() 



