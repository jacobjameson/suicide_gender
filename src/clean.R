################################################################################
# AUTHOR: J. Jameson

# DESCRIPTION: This script reads in the raw data files and cleans them for
# analysis. The cleaned data is then saved as a .csv file in the outputs/data
# folder. The script also contains a function to impute missing values using
# XGBoost. 
################################################################################
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 

#-------------------------------------------------------------------------------
# Prepare data for merging -------------------------------------------

scores <- read_csv("raw/2023_scorecard.csv")

scores <- scores %>%
  mutate(grade = case_when(
    Grade == 'A' ~ "More Strict",
    Grade == 'A-' ~ "More Strict",
    Grade == 'B+' ~ "More Strict",
    Grade == 'B' ~ "More Strict",
    Grade == 'B-' ~ "More Strict",
    Grade == 'C+' ~ "More Strict",
    TRUE ~ 'Less Strict')
    ) %>%
  dplyr::select(State, grade, Grade)

## aggregate data used to improve estimates:
suicides.aggregate <- read.csv(
  "raw/suicide_reports_aggregate.csv"
)

end <- nrow(suicides.aggregate) - 16
suicides.aggregate <- suicides.aggregate[c(1 : end),]

suicides.aggregate$Deaths <- gsub("\\*\\*", "", suicides.aggregate$Deaths) 

suicides.aggregate$Deaths <- gsub(",", "", suicides.aggregate$Deaths)
suicides.aggregate$Population <- gsub(",", "", suicides.aggregate$Population) 
suicides.aggregate$Population <- as.numeric(suicides.aggregate$Population)

suicides.aggregate$Deaths <- ifelse(
  suicides.aggregate$Deaths == '--', 
  5, 
  suicides.aggregate$Deaths
)

suicides.aggregate$Deaths <- as.numeric(suicides.aggregate$Deaths)
suicides.aggregate$Year <- as.numeric(suicides.aggregate$Year)

suicides.aggregate <- suicides.aggregate %>%
  mutate(Age.Group = ifelse(
    Age.Group == '75 to 79', '75 to 80', Age.Group)) %>%
  select('Age.Group', 'State', agg_deaths_by_agegroup = 'Deaths', 'Year')

#-------------------------------------------------------------------------------
# Cleaning function ------------------------------------------------------------

clean_suicide_data <- function(file_path, replace_suppressed = NA) {

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
    replace_suppressed, 
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
  
  suicide_data <- suicide_data %>%
    dplyr::select(State, Year, Age, Sex, Deaths, Population, Grade) %>%
    filter(Age >= 12, Age <= 80)
  
  breaks <- c(10, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, Inf)
  
  # Define the labels for the age groups
  labels <- c("10 to 14", "15 to 19", "20 to 24", "25 to 29", 
              "30 to 34", "35 to 39", 
              "40 to 44", "45 to 49", "50 to 54", 
              "55 to 59", "60 to 64", 
              "65 to 69", "70 to 74", "75 to 80")
  
  # Create the Age.Group column
  suicide_data$Age.Group <- cut(suicide_data$Age, breaks = breaks, 
                                labels = labels, right = FALSE)
  
  suicide_data$Age.Group <- as.character(suicide_data$Age.Group)
  
  suicide_data <- merge(suicide_data, suicides.aggregate,
                        on=c('State', 'Year', 'Age.Group')) %>%
    dplyr:: select(-Age.Group)
  
  return(suicide_data)
}


#-------------------------------------------------------------------------------
# Read in and clean data -------------------------------------------------------

## firearm suicides
suicide.firearm <- clean_suicide_data(
  "raw/suicide_reports_firearm.csv"
  )
suicide.firearm <- suicide.firearm %>%
  rename(FirearmDeaths = Deaths)

## non-firearm suicides
suicide.nonfirearm <- clean_suicide_data(
  "raw/suicide_reports_nonfirearm.csv"
  )

suicide.nonfirearm <- suicide.nonfirearm %>%
  rename(NonFirearmDeaths = Deaths)

## total suicides
suicide.all <- clean_suicide_data(
  "raw/suicide_reports_all.csv"
  )

#-------------------------------------------------------------------------------
# XGBoost Imputation
source('src/imputation analysis.R')


suicide.firearm <- xg_imputation_verbose(suicide.firearm, 'FirearmDeaths')
suicide.nonfirearm <- xg_imputation_verbose(suicide.nonfirearm, 'NonFirearmDeaths')
suicide.all <- xg_imputation_verbose(suicide.all, 'Deaths')

add_categories <- function(data){
  
  data <- merge(data, scores, by = c('State', 'Grade'))
  data$Year <- as.numeric(data$Year)
  
  data$AgeGroup <- cut(
    data$Age,
    breaks = c(12, 20, 30, 40, 50, 60, 70, 80),
    labels = c('12–20', '21–30', '31–40', '41–50',
               '51–60', '61-70', '71-80'),
    include.lowest = TRUE)
  
  data$YearGroup <- cut(
    data$Year,
    breaks = c(2000, 2014, 2021),
    labels = c("2001-2014", "2015-2021"),
    include.lowest = TRUE
  )
  return(data)
}

suicide.firearm <- add_categories(suicide.firearm)
suicide.nonfirearm <- add_categories(suicide.nonfirearm)
suicide.all <- add_categories(suicide.all)

suicide.firearm$FirearmDeaths_Imputed <- case_when(
  is.na(suicide.firearm$FirearmDeaths) ~ suicide.firearm$Predictions,
  TRUE ~ suicide.firearm$FirearmDeaths)

suicide.nonfirearm$NonFirearmDeaths_Imputed <- case_when(
  is.na(suicide.nonfirearm$NonFirearmDeaths) ~ suicide.nonfirearm$Predictions,
  TRUE ~ suicide.nonfirearm$NonFirearmDeaths)

suicide.all$Deaths_Imputed <- case_when(
  is.na(suicide.all$Deaths) ~ suicide.all$Predictions,
  TRUE ~ suicide.all$Deaths)

write.csv(suicide.firearm, 'outputs/data/suicide_firearm_cleaned.csv')
write.csv(suicide.nonfirearm, 'outputs/data/suicide_nonfirearm_cleaned.csv')
write.csv(suicide.all, 'outputs/data/suicide_all_cleaned.csv')

#-------------------------------------------------------------------------------
# Compute crude deaths per 100k for each age -----------------------------------
# Do this for both imputed and non-imputed values ------------------------------

## firearm suicides deaths per 100k
avg_deaths.firearm.1 <-
  suicide.firearm %>%
  filter(is.na(FirearmDeaths) == F) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'Firearm')
  
avg_deaths.firearm.2 <-
  suicide.firearm %>%
  filter(is.na(FirearmDeaths) == T) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'Firearm')

avg_deaths.firearm <- merge(avg_deaths.firearm.1, avg_deaths.firearm.2)

#----------

## Nonfirearm suicides deaths per 100k
avg_deaths.nonfirearm.1 <-
  suicide.nonfirearm %>%
  filter(is.na(NonFirearmDeaths) == F) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'NonFirearm')

avg_deaths.nonfirearm.2 <-
  suicide.nonfirearm %>%
  filter(is.na(NonFirearmDeaths) == T) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'NonFirearm')

avg_deaths.nonfirearm <- merge(avg_deaths.nonfirearm.1, avg_deaths.nonfirearm.2)

#----------

## All suicides deaths per 100k
avg_deaths.all.1 <-
  suicide.all %>%
  filter(is.na(Deaths) == F) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'All')

avg_deaths.all.2 <-
  suicide.all %>%
  filter(is.na(Deaths) == T) %>%
  group_by(Age, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'All')

avg_deaths.all <- merge(avg_deaths.all.1, avg_deaths.all.2)

#----------
## bind together
avg_deaths <- bind_rows(
  avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all
)

write.csv(avg_deaths, 'outputs/data/deaths_cleaned_age.csv')

#-------------------------------------------------------------------------------
# Compute crude deaths per 100k for each age decile ----------------------------

## firearm suicides deaths per 100k
avg_deaths.firearm.1 <-
  suicide.firearm %>%
  filter(is.na(FirearmDeaths) == F) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'Firearm')

avg_deaths.firearm.2 <-
  suicide.firearm %>%
  filter(is.na(FirearmDeaths) == T) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'Firearm')

avg_deaths.firearm <- merge(avg_deaths.firearm.1, avg_deaths.firearm.2)

#----------

## Nonfirearm suicides deaths per 100k
avg_deaths.nonfirearm.1 <-
  suicide.nonfirearm %>%
  filter(is.na(NonFirearmDeaths) == F) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'NonFirearm')

avg_deaths.nonfirearm.2 <-
  suicide.nonfirearm %>%
  filter(is.na(NonFirearmDeaths) == T) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'NonFirearm')

avg_deaths.nonfirearm <- merge(avg_deaths.nonfirearm.1, avg_deaths.nonfirearm.2)

#----------

## All suicides deaths per 100k
avg_deaths.all.1 <-
  suicide.all %>%
  filter(is.na(Deaths) == F) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'All')

avg_deaths.all.2 <-
  suicide.all %>%
  filter(is.na(Deaths) == T) %>%
  group_by(AgeGroup, grade, Sex, YearGroup) %>%
  summarise(AvgDeathsPer100k_impute = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>%
  ungroup() %>%
  mutate(category = 'All')

avg_deaths.all <- merge(avg_deaths.all.1, avg_deaths.all.2)

#----------
## bind together
avg_deaths <- bind_rows(
  avg_deaths.firearm, avg_deaths.nonfirearm, avg_deaths.all
)

write.csv(avg_deaths, 'outputs/data/deaths_cleaned_age_decile.csv')
################################################################################