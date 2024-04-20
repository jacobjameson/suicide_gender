################################################################################
# AUTHOR: J. Jameson

# DESCRIPTION: This script generates the figures for the manuscript.
################################################################################
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 
library(ggthemes) # for additional themes
library(RColorBrewer) # for color palettes

# Load data --------------------------------------------------------------------
avg_deaths <- read_csv("outputs/data/deaths_cleaned_age.csv")

#-------------------------------------------------------------------------------
# Theme ------------------------------------------------------------------------

pal <- c("#E31A1C","#1f78b4")

cap.1 <- "Source: Centers for Disease Control and Prevention 
          Web-based Injury Statistics Query and Reporting 
          System (WISQARS), March 2024. 'More Strict' 
          and 'Less Strict' refer to Annual Gun Law Scorecard grades as described in text. 
          Trend lines represent smoothed averages of deaths per 100k
          by age, using a LOESS method for non-parametric local 
          regression. Suppressed values (suggesting suicide counts between one and nine or based on specific 
          statistical criteria) have been dropped."

cap.2 <- "Source: Centers for Disease Control and Prevention 
          Web-based Injury Statistics Query and Reporting 
          System (WISQARS), March 2024. 'More Strict' 
          and 'Less Strict' refer to Annual Gun Law Scorecard grades as described in text. 
          Trend lines represent smoothed averages of the differences in deaths per 100k
          by age in 'More Strict' vs 'Less Strict' steates, using a LOESS method for non-parametric local 
          regression. For suppressed values denoted 
          by '--' (suggesting suicide counts between one and nine or based on specific 
          statistical criteria), we have imputed an estimate using the XGBoost model described in text."

#-------------------------------------------------------------------------------

deaths.all <- read_csv("outputs/data/suicide_all_cleaned.csv")[, -1]
deaths.firearm <- read_csv("outputs/data/suicide_firearm_cleaned.csv")[, -1]
deaths.nonfirearm <- read_csv("outputs/data/suicide_nonfirearm_cleaned.csv")[, -1]

deaths.all <- filter(deaths.all, YearGroup == '2015-2021')
deaths.firearm <- filter(deaths.firearm, YearGroup == '2015-2021')
deaths.nonfirearm <- filter(deaths.nonfirearm, YearGroup == '2015-2021')


deaths.all <- deaths.all %>%
  group_by(Age, grade, Sex) %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / sum(Population) * 100000) %>%
  spread(key = grade, value = DeathsPer100k) %>%
  mutate(Difference = `Less Strict` - `More Strict`,
         type = 'All Suicide') 
  
deaths.firearm <- deaths.firearm %>%
  group_by(Age, grade, Sex) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / sum(Population) * 100000) %>%
  spread(key = grade, value = DeathsPer100k) %>%
  mutate(Difference = `Less Strict` - `More Strict`,
         type = 'Firearm Suicide') 

deaths.nonfirearm <- deaths.nonfirearm %>%
  group_by(Age, grade, Sex) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / sum(Population) * 100000) %>%
  spread(key = grade, value = DeathsPer100k) %>%
  mutate(Difference = `Less Strict` - `More Strict`,
         type = 'Non-Firearm Suicide')

deaths <- rbind(deaths.all, deaths.firearm, deaths.nonfirearm)

deaths %>%
  mutate(Sex = case_when(
    Sex == 'Males' ~ 'Male Differences',
    TRUE ~ 'Female Differences'
  )) %>%
  ggplot(aes(x = Age, 
             y = Difference, 
             color = Sex)) +
  geom_hline(yintercept = 0, linetype='dashed') +
  xlim(12,80) + 
  geom_smooth(aes(fill = Sex), size = 1, method = "loess") + 
  facet_wrap(~type) + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  labs(title = "Differences in Suicide Deaths per 100,000 by Age \nAcross More and Less Gun-Strict States",
       subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
       caption = str_wrap(cap.2, 150),
       x = "Age",
       y = "Differences in Suicides \nper 100,000",
       color = "Sex",
       fill = "Sex") +
  theme_minimal(base_size = 16) +  
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', color = 'black'),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'bottom',
    plot.caption = element_text(size = 9, hjust = 0),
    axis.title = element_text(size = 20, color = 'black'),  
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) 


ggsave("outputs/figures/figure_1_impute.png", width = 10, height = 7, dpi = 300)

# Plotting code ----------------------------------------------------------------

avg_deaths %>%
  filter(YearGroup == "2015-2021") %>%
  mutate(Sex = case_when(
    Sex == 'Males' ~ 'Male Differences',
    TRUE ~ 'Female Differences'
  )) %>%
  ggplot(aes(x = Age, 
             y = AvgDeathsPer100k, 
             color = Sex, 
             linetype = grade)) +
  geom_smooth(aes(fill = Sex), size = 1, method = "loess") + 
  facet_wrap(~category) + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  labs(title = "Average Suicide Deaths per 100,000 by Age \n(Dropped Suppressed Values)",
       subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
       caption = str_wrap(cap.1, 150),
       x = "Age",
       y = "Average Suicide Deaths \nper 100,000",
       color = "Sex",
       fill = "Sex",
       linetype = "Annual Gun Law Scorecard") +
  theme_minimal(base_size = 16) +  
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', color = 'black'),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'bottom',
    plot.caption = element_text(size = 9, hjust = 0),
    axis.title = element_text(size = 20, color = 'black'),  
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) 

ggsave("outputs/figures/figure_1_dropped.png", width = 10, height = 7, dpi = 300)


deaths %>%
  mutate(type = case_when(type == 'All' ~ 'All Suicides',
                   type == 'Firearm' ~ 'Firearm Suicides',
                   type == 'Non-Firearm' ~ 'Non-Firearm Suicides')) %>%
  ggplot(aes(x = Age, 
             y = Difference, 
             color = Sex)) +
  geom_hline(yintercept = 0, linetype='dashed') +
  xlim(12,80) + 
  geom_smooth(aes(fill = Sex), size = 1, method = "loess") + 
  facet_wrap(~type) + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  labs(title = "Differences in Suicide Deaths per 100,000 by Age \nAcross Strict and Non-Strict States",
       subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
       caption = str_wrap(cap.2, 170),
       x = "Age",
       y = "Difference in Suicides \nper 100,000",
       color = "Sex",
       fill = "Sex") 

ggsave("outputs/figures/figure_1_impute.png", width = 10, height = 6, dpi = 300)


avg_deaths %>%
  filter(YearGroup == "2015-2021") %>%
  ggplot(aes(x = Age, 
             y = AvgDeathsPer100k_impute, 
             color = Sex,
             linetype = grade)) +
  geom_smooth(aes(fill = Sex), size = 1, method = "loess") + 
  facet_wrap(~category) + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  labs(title = "Average Suicide Deaths per 100,000 by Age",
       subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
       caption = str_wrap(cap.2, 170),
       x = "Age",
       y = "Average Suicide Deaths \nper 100,000",
       color = "Sex",
       fill = "Sex",
       linetype = "Annual Gun Law Scorecard") 

ggsave("outputs/figures/figure3_v2.png", width = 10, height = 6, dpi = 300)



deaths.all <- read_csv("outputs/data/suicide_all_cleaned.csv")[, -1]
deaths.firearm <- read_csv("outputs/data/suicide_firearm_cleaned.csv")[, -1]
deaths.nonfirearm <- read_csv("outputs/data/suicide_nonfirearm_cleaned.csv")[, -1]

deaths.all <- deaths.all %>%
  group_by(Sex, Age, YearGroup) %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() %>%
  mutate(type = 'All Suicides')

deaths.firearm <- deaths.firearm %>%
  group_by(Sex, Age, YearGroup) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() %>%
  mutate(type = 'Firearm Suicides')

deaths.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, YearGroup) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() %>%
  mutate(type = 'Non-Firearm Suicides')

deaths <- rbind(deaths.all, deaths.firearm, deaths.nonfirearm)

deaths %>%
  ggplot(aes(x = Age, 
             y = DeathsPer100k, 
             color = Sex,
             linetype = YearGroup)) +
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) + 
  facet_wrap(~type, ncol = 3) +
  geom_line(size =0.75) +
  labs(title = "Firearm Suicide Deaths per 100,000 by Age",
       subtitle = "Data Years: 2001 to 2021, United States, All Races, All Ethnicities",
       caption = str_wrap("Source: Centers for Disease Control and Prevention 
          Web-based Injury Statistics Query and Reporting 
          System (WISQARS), March 2024. For suppressed values denoted 
          by '--' (suggesting suicide counts between one and nine or based on specific 
          statistical criteria), we have imputed an estimate using the XGBoost model described in text.", 130),
       x = "Age",
       y = "Deaths per 100,000\n",
       color = "Sex",
       linetype = 'Years',
       fill = "Sex") +
  theme_minimal(base_size = 16) +  
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', color = 'black'),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'right',
    plot.caption = element_text(size = 9, hjust = 0),
    axis.title = element_text(size = 20, color = 'black'),  
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) 


ggsave("outputs/figures/figure_2_impute.png", width = 11, height = 6, dpi = 300)



#-------------------------------------------------------------------------------

suicides <- read_csv('outputs/data/suicide_firearm_cleaned.csv')

suicides %>%
  filter(is.na(FirearmDeaths) == F, YearGroup == '2015-2021') %>%
  group_by(FirearmDeaths) %>%
  summarise(
    MeanPrediction = mean(Predictions, na.rm = TRUE),
    SE = sd(Predictions, na.rm = TRUE) / sqrt(n()),
    LowerCI = MeanPrediction - 1.96 * SE, # 95% CI lower bound
    UpperCI = MeanPrediction + 1.96 * SE  # 95% CI upper bound
  ) %>%
  ggplot(aes(x = FirearmDeaths, y = MeanPrediction)) +
  geom_point(size = 1, color = "#E31A1C") +
  geom_errorbar(
    aes(ymin = LowerCI, ymax = UpperCI), 
    color = '#E31A1C', width=0.4,
    alpha = .9,
  ) + 
  geom_abline(
    intercept = 0, 
    slope = 1, 
    linetype = "dashed", 
    color = "black"
  ) + 
  labs(
    title = "Predicted vs Actual Firearm Suicides",
    subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
    y = "Predicted Firearm Suicides\n",
    x = "Actual Firearm Suicides"
  ) +
  geom_rect(
    aes(xmin = 1, xmax = 9, ymin = -Inf, ymax = Inf), 
    fill = "grey", 
    alpha = 0.009
  ) +
  annotate(
    "text", 
    x = 5, 
    y = 30, 
    label = "Suppressed \nData", 
    vjust = 2, 
    size = 5, 
    fontface = 'bold'
  ) +
  annotate(
    "text", 
    x = 20, 
    y = 12, 
    label = "Mean Estimate and \n95% Prediction Interval", 
    color = "#E31A1C", 
    fontface = 'bold'
  ) +
  annotate(
    "text", 
    x = 17, 
    y = 25, 
    label = "Dashed line: \nPredicted = Actual", 
    color = "black", 
    fontface = 'bold'
  ) +
  geom_segment(
    aes(x = 20, y = 14, xend = 20, yend = 17), 
    color = "#E31A1C", 
    arrow = arrow(type = "open", length = unit(0.05, "inches"))
  ) +
  geom_segment(
    aes(x = 20, y = 25, xend = 24, yend = 25), 
    color = "black", 
    arrow = arrow(type = "open", length = unit(0.05, "inches"))
  ) +
  geom_label(
    aes(x = 34, y = 4), 
    hjust = 0,
    label = sprintf("RMSE: 1.41\nR²: 0.95"),
    fill = "white",
    color = "black",
    size = 6
  ) + geom_smooth(method = 'loess', se = F, color = "#E31A1C") +
  theme_minimal(base_size = 16) +  
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05)), limits = c(0,40.5)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), limits = c(0,44.5)) +
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', color = 'black'),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'right',
    plot.caption = element_text(size = 9, hjust = 0),
    axis.title = element_text(size = 20, color = 'black'),  
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) 

ggsave("outputs/figures/predicted_v_actual.png", width = 10, height = 7, dpi = 300)


suicides <- read_csv('outputs/data/suicide_nonfirearm_cleaned.csv')

suicides %>%
  filter(!is.na(NonFirearmDeaths), YearGroup == '2015-2021') %>%
  group_by(NonFirearmDeaths) %>%
  summarise(
    MeanPrediction = mean(Predictions, na.rm = TRUE),
    SE = sd(Predictions, na.rm = TRUE) / sqrt(n()),
    LowerCI = MeanPrediction - 1.96 * SE, # 95% CI lower bound
    UpperCI = MeanPrediction + 1.96 * SE  # 95% CI upper bound
  ) %>%
  ggplot(aes(x = NonFirearmDeaths, y = MeanPrediction)) +
  geom_point(size = 2, color = "#0072B5") +
  geom_errorbar(
    aes(ymin = LowerCI, ymax = UpperCI), 
    color = '#0072B5', 
    alpha = .9
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1, 
    linetype = "dashed", 
    color = "#BC3C29"
  ) +
  xlim(-0.5, 31.5) +
  ylim(-0.5, 33) +
  labs(
    title = "Predicted vs Actual Non-Firearm Suicides",
    subtitle = "Data Years: 2015 to 2021, United States, All Races, All Ethnicities",
    y = "Predicted Non-Firearm Suicide",
    x = "Actual Non-Firearm Suicide"
  ) +
  geom_rect(
    aes(xmin = 1, xmax = 9, ymin = -Inf, ymax = Inf), 
    fill = "grey", 
    alpha = 0.009
  ) +
  annotate(
    "text", 
    x = 5, 
    y = 20, 
    label = "Suppressed Data", 
    vjust = 2, 
    size = 5, 
    fontface = 'bold'
  ) +
  annotate(
    "text", 
    x = 20, 
    y = 12, 
    label = "Mean Estimate and \n95% CI for Predicted Value", 
    color = "#0072B5", 
    fontface = 'bold'
  ) +
  annotate(
    "text", 
    x = 17, 
    y = 25, 
    label = "Red dashed line: \nPredicted = Actual", 
    color = "#BC3C29", 
    fontface = 'bold'
  ) +
  geom_segment(
    aes(x = 20, y = 14, xend = 20, yend = 17), 
    color = "#0072B5", 
    arrow = arrow(type = "open", length = unit(0.05, "inches"))
  ) +
  geom_segment(
    aes(x = 20, y = 25, xend = 24, yend = 25), 
    color = "#BC3C29", 
    arrow = arrow(type = "open", length = unit(0.05, "inches"))
  ) +
  geom_label(
    aes(x = 26, y = 2), 
    hjust = 0,
    label = sprintf("RMSE: 1.39\nR²: 0.95"),
    fill = "white",
    color = "black",
    size = 6
  )


ggsave("outputs/figures/predicted_v_actual_nonfirearm.png", width = 10, height = 6, dpi = 300)





#-------------------------------------------------------------------------------

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
  mutate(grouping = case_when(
    Sex == 'Males' & grade == 'More Strict' ~ 'Male More Strict',
    Sex == 'Females' & grade == 'Less Strict' ~ 'Females Less Strict'
  )) %>%
  filter(is.na(grouping) == F) %>%
  group_by(grouping, category) %>%
  summarize(AvgDeaths = mean(AvgDeathsPer100k, na.rm = TRUE)) %>%
  ungroup() %>%
  # Create a wider format to prepare for difference calculation
  spread(key = grouping, value = AvgDeaths) %>%
  # Compute the difference between "More Strict" and "Less Strict"
  mutate(`Difference Male More Strict AvgDeathsPer100k - Female Less Strict AvgDeathsPer100k` = `Male More Strict` - `Females Less Strict`) %>%
  # Select only the relevant columns for the output
  select(category, `Difference Male More Strict AvgDeathsPer100k - Female Less Strict AvgDeathsPer100k`)

# Perform a significance test for each group
# Assuming you have a binary variable for strictness in the original data
# This example uses a t-test as a placeholder
significance_tests <- avg_deaths %>%
  mutate(grouping = case_when(
    Sex == 'Males' & grade == 'More Strict' ~ 'Male More Strict',
    Sex == 'Females' & grade == 'Less Strict' ~ 'Females Less Strict'
  )) %>%
  filter(is.na(grouping) == F) %>%
  group_by(category) %>%
  summarize(
    t_value = t.test(AvgDeathsPer100k ~ grouping)$statistic,
    p_value = t.test(AvgDeathsPer100k ~ grouping)$p.value
  ) %>%
  ungroup()

# Merge the average differences with significance test results
final_results <- left_join(avg_differences, significance_tests, by = c("category"))

# Inspect the final results
print(final_results)

write.csv(final_results, "outputs/differences_by_sexandpolicy_total.csv", row.names = FALSE)

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




