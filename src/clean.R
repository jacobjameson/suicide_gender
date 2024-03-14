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
    Grade == 'A' ~ "More Strict",
    Grade == 'A-' ~ "More Strict",
    Grade == 'B+' ~ "More Strict",
    Grade == 'B' ~ "More Strict",
    Grade == 'B-' ~ "More Strict",
    Grade == 'C+' ~ "More Strict",
    TRUE ~ 'Less Strict')
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
  
  suicide_data$AgeGroup <- cut(
    suicide_data$Age,
    breaks = c(12, 20, 30, 40, 50, 60, 70, 80),
    labels = c('12–20', '21–30', '31–40', '41–50',
               '51–60', '61-70', '71-80'),
    include.lowest = TRUE)
  
  suicide_data$YearGroup <- cut(
    suicide_data$Year,
    breaks = c(2000, 2014, 2021),
    labels = c("2001-2014", "2015-2021"),
    include.lowest = TRUE
  )
  
  suicide_data <- suicide_data %>%
    select(State, Year, Age, AgeGroup, Sex, Deaths,
           Population, grade, State, YearGroup) %>%
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
       color = "Gender",
       fill = "Gender",
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

avg_deaths.firearm <- suicide.firearm %>%
  group_by(AgeGroup, grade, Sex, YearGroup, State) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Firearm')

avg_deaths.nonfirearm <- suicide.nonfirearm %>%
  group_by(AgeGroup, grade, Sex, YearGroup, State) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Non-Firearm')

avg_deaths.all <- suicide.all %>%
  group_by(AgeGroup, grade, Sex, YearGroup, State) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'All')


avg_deaths <- bind_rows(
  avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all
  ) %>% filter(YearGroup == '2015-2021')


# generate summary statistics for the average deaths per 100,000 by Sex and Category for different age groups
glimpse(avg_deaths)

library(dplyr)

# Assuming 'grade' contains the "Less Strict" vs "More Strict" designations
# Compute average differences within each AgeGroup, Sex, and category
avg_differences <- avg_deaths %>%
  group_by(Sex, category, grade) %>%
  summarize(AvgDeaths = mean(AvgDeathsPer100k, na.rm = TRUE)) %>%
  ungroup() %>%
  # Create a wider format to prepare for difference calculation
  spread(key = grade, value = AvgDeaths) %>%
  # Compute the difference between "More Strict" and "Less Strict"
  mutate(`Difference (More Strict AvgDeathsPer100k - Less Strict AvgDeathsPer100k` = `More Strict` - `Less Strict`) %>%
  # Select only the relevant columns for the output
  select(Sex, category, `Difference (More Strict AvgDeathsPer100k - Less Strict AvgDeathsPer100k`)

# Perform a significance test for each group
# Assuming you have a binary variable for strictness in the original data
# This example uses a t-test as a placeholder
significance_tests <- avg_deaths %>%
  group_by(Sex, category) %>%
  summarize(
    t_value = t.test(AvgDeathsPer100k ~ grade)$statistic,
    p_value = t.test(AvgDeathsPer100k ~ grade)$p.value
  ) %>%
  ungroup()

# Merge the average differences with significance test results
final_results <- left_join(avg_differences, significance_tests, by = c("Sex", "category"))

# Inspect the final results
print(final_results)

write.csv(final_results, "outputs/differences_by_policyenv_totals.csv", row.names = FALSE)

avg_deaths.firearm <- suicide.firearm %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Firearm')

avg_deaths.nonfirearm <- suicide.nonfirearm %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Non-Firearm')

avg_deaths.all <- suicide.all %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'All')


avg_deaths <- bind_rows(avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all)

glimpse(avg_deaths)


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




avg_deaths.firearm <- suicide.firearm %>%
  group_by(Age, State, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop',) %>%
  mutate(category = 'Firearm')

avg_deaths.nonfirearm <- suicide.nonfirearm %>%
  group_by(Age, State, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'Non-Firearm')

avg_deaths.all <- suicide.all %>%
  group_by(Age, State, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(Deaths, na.rm=T) / 
              sum(Population, na.rm=T) * 100000,
            .groups = 'drop') %>%
  mutate(category = 'All')

avg_deaths <- bind_rows(avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all)

avg_deaths <- merge(avg_deaths, scores, on=c('State'))

avg_deaths.male <- avg_deaths %>%
  filter(Sex == 'Males', YearGroup == '2015-2021',
         category == 'Firearm')
  

library(nlme)
library(geepack)

total_suicide_model <- gls(AvgDeathsPer100k ~ grade, 
                           data = avg_deaths.male,
                           correlation = corExchangeable(form = ~ 1 | State))

# Example GEE for life-course decades
gee_model <- geeglm(AvgDeathsPer100k ~ PolicyEnv * AgeGroup, 
                    id = State, 
                    data = avg_deaths_processed, 
                    family = gaussian(link = "identity"), 
                    corstr = "exchangeable")

# Plotting the smoothed trends
ggplot(avg_deaths_processed, aes(x = as.numeric(AgeGroup), y = AvgDeathsPer100k)) +
  geom_smooth(aes(color = PolicyEnv), method = "loess") +
  theme_minimal()




