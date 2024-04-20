################################################################################
# AUTHOR: J. Jameson

# DESCRIPTION: This script creates tables for the manuscript.
################################################################################
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 

# Load data --------------------------------------------------------------------
deaths.all <- read_csv("outputs/data/suicide_all_cleaned.csv")[, -1]
deaths.firearm <- read_csv("outputs/data/suicide_firearm_cleaned.csv")[, -1]
deaths.nonfirearm <- read_csv("outputs/data/suicide_nonfirearm_cleaned.csv")[, -1]

deaths.all <- filter(deaths.all, YearGroup == '2015-2021')
deaths.firearm <- filter(deaths.firearm, YearGroup == '2015-2021')
deaths.nonfirearm <- filter(deaths.nonfirearm, YearGroup == '2015-2021')

################################################################################
# -----------------------------------------------------------------------------
# Summary statistics ----------------------------------------------------------
# Males vs Females by Age and Age Decile

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(Deaths) == F) %>%
  summarize(DeathsPer100k = sum(Deaths) / 
           sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males",
    imputed_missing = "No",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
    ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(FirearmDeaths) == F) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males",
    imputed_missing = "No",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(NonFirearmDeaths) == F) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males",
    imputed_missing = "No",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_test.suppressed <- bind_rows(t_test.all, t_test.firearm, t_test.nonfirearm) 

# Imputed values ------------------------------------------------

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males",
    imputed_missing = "Yes",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males",
    imputed_missing = "Yes",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males",
    imputed_missing = "Yes",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_tests <- bind_rows(t_test.suppressed, 
                      t_test.all, t_test.firearm, t_test.nonfirearm) 

################################################################################
# Males vs Females by Age Group

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, AgeGroup, State) %>%
  filter(is.na(Deaths) == F) %>%
  summarize(DeathsPer100k = sum(Deaths) / 
              sum(Population) * 100000) %>% 
    ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

for (agegroup in unique(differences.all$AgeGroup)) {
  
  subset <- differences.all[differences.all$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for ALL Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "No",
      type = 'All',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, AgeGroup, State) %>%
  filter(is.na(FirearmDeaths) == F) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% 
  ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

for (agegroup in unique(differences.firearm$AgeGroup)) {
  
  subset <- differences.firearm[differences.firearm$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for Firearm Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "No",
      type = 'Firearm',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}

## NonFirearm suicides ----
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, AgeGroup, State) %>%
  filter(is.na(NonFirearmDeaths) == F) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% 
  ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

for (agegroup in unique(differences.nonfirearm$AgeGroup)) {
  
  subset <- differences.nonfirearm[differences.nonfirearm$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for Non-Firearm Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "No",
      type = 'Non-Firearm',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}

# Imputed values ------------------------------------------------

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, AgeGroup, State) %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% 
  ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

for (agegroup in unique(differences.all$AgeGroup)) {
  
  subset <- differences.all[differences.all$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for ALL Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "Yes",
      type = 'All',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, AgeGroup, State) %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% 
  ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

for (agegroup in unique(differences.firearm$AgeGroup)) {
  
  subset <- differences.firearm[differences.firearm$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for Firearm Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "Yes",
      type = 'Firearm',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}

## NonFirearm suicides ----
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, AgeGroup, State) %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% 
  ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

for (agegroup in unique(differences.nonfirearm$AgeGroup)) {
  
  subset <- differences.nonfirearm[differences.nonfirearm$AgeGroup == agegroup, ]
  
  temp <- broom::tidy(
    t.test(subset[subset$Sex == 'Females', ]$DeathsPer100k, 
           subset[subset$Sex == 'Males', ]$DeathsPer100k)) %>% 
    mutate(
      description = paste("T-test for Non-Firearm Suicides Per 100k between Females and Males", 
                          "for", agegroup, "age group"),
      imputed_missing = "Yes",
      type = 'Non-Firearm',
      n_obs_females = n_females,
      n_obs_males = n_males,
    ) %>%
    rename(DeathsPer100k_Female = estimate1,
           DeathsPer100k_Male = estimate2,
           Difference = estimate)
  
  t_tests <- bind_rows(t_tests, temp)
}

# Save the results as a CSV
write.csv(t_tests, "outputs/tables/comparisons_by_sex_2015-2021.csv", 
          row.names = FALSE)
################################################################################
# -----------------------------------------------------------------------------
# Summary statistics ----------------------------------------------------------
# Males vs Females within grades

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(Deaths) == F, grade == 'More Strict') %>%
  summarize(DeathsPer100k = sum(Deaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males in More Strict States",
    imputed_missing = "No",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(FirearmDeaths) == F, grade == 'More Strict') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males in More Strict States",
    imputed_missing = "No",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(NonFirearmDeaths) == F, grade == 'More Strict') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males in More Strict States",
    imputed_missing = "No",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_test.suppressed <- bind_rows(t_test.all, t_test.firearm, t_test.nonfirearm) 

# Imputed values ------------------------------------------------

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'More Strict') %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males in More Strict States",
    imputed_missing = "Yes",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'More Strict') %>%

  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males in More Strict States",
    imputed_missing = "Yes",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'More Strict') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males in More Strict States",
    imputed_missing = "Yes",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'More Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_tests.1 <- bind_rows(t_test.suppressed, 
                     t_test.all, t_test.firearm, t_test.nonfirearm) 


## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(Deaths) == F, grade == 'Less Strict') %>%
  summarize(DeathsPer100k = sum(Deaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males in Less Strict States",
    imputed_missing = "No",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(FirearmDeaths) == F, grade == 'Less Strict') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males in Less Strict States",
    imputed_missing = "No",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  filter(is.na(NonFirearmDeaths) == F, grade == 'Less Strict') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males in Less Strict States",
    imputed_missing = "No",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_test.suppressed <- bind_rows(t_test.all, t_test.firearm, t_test.nonfirearm) 

# Imputed values ------------------------------------------------

## All suicides ----
differences.all <- deaths.all %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'Less Strict') %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.all$Sex == 'Females')
n_males <- sum(differences.all$Sex == 'Males')

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$Sex == 'Females', ]$DeathsPer100k, 
         differences.all[differences.all$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Males in Less Strict States",
    imputed_missing = "Yes",
    type = 'All',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'Less Strict') %>%
  
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.firearm$Sex == 'Females')
n_males <- sum(differences.firearm$Sex == 'Males')

t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Males in Less Strict States",
    imputed_missing = "Yes",
    type = 'Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(Sex, Age, State) %>%
  filter(grade == 'Less Strict') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

n_females <- sum(differences.nonfirearm$Sex == 'Females')
n_males <- sum(differences.nonfirearm$Sex == 'Males')

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$Sex == 'Females', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$Sex == 'Males', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Non-Firearm Per 100k between Females and Males in Less Strict States",
    imputed_missing = "Yes",
    type = 'Non-Firearm',
    n_obs_females = n_females,
    n_obs_males = n_males,
    grade = 'Less Strict'
  ) %>%
  rename(DeathsPer100k_Female = estimate1,
         DeathsPer100k_Male = estimate2,
         Difference = estimate)

t_tests <- bind_rows(t_tests.1, t_test.suppressed, 
                       t_test.all, t_test.firearm, t_test.nonfirearm) 

write.csv(t_tests, "outputs/tables/comparisons_by_sex_2015-2021_within_grade.csv", 
          row.names = FALSE)

################################################################################
# Summary statistics ----------------------------------------------------------
# Males vs Males across grades

## All suicides ----
differences.all <- deaths.all %>%
  group_by(grade, Age, State) %>%
  filter(is.na(Deaths) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(Deaths) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$grade == 'More Strict', ]$DeathsPer100k, 
         differences.all[differences.all$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "No",
    type = 'All',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(FirearmDeaths) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 


t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "No",
    type = 'Firearm',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)


## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(NonFirearmDeaths) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for NonFirearm Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "No",
    type = 'NonFirearm',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)

t_test.suppressed <- bind_rows(t_test.all, t_test.firearm, t_test.nonfirearm) 

# Females vs Females across grades

## All suicides ----
differences.all <- deaths.all %>%
  group_by(grade, Age, State) %>%
  filter(is.na(Deaths) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(Deaths) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$grade == 'More Strict', ]$DeathsPer100k, 
         differences.all[differences.all$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "No",
    type = 'All',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(FirearmDeaths) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 


t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "No",
    type = 'Firearm',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)


## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(NonFirearmDeaths) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for NonFirearm Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "No",
    type = 'NonFirearm',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)

t_test.suppressed <- bind_rows(t_test.suppressed,
                               t_test.all, t_test.firearm, t_test.nonfirearm) 


# Imputed values ------------------------------------------------

## All suicides ----
differences.all <- deaths.all %>%
  group_by(grade, Age, State) %>%
  filter(is.na(Deaths_Imputed) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$grade == 'More Strict', ]$DeathsPer100k, 
         differences.all[differences.all$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'All',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(FirearmDeaths_Imputed) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 


t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'Firearm',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)


## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(NonFirearmDeaths_Imputed) == F, Sex == 'Males') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for NonFirearm Suicides Per 100k between Males and Males in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'NonFirearm',
    sex = 'Males'
  ) %>%
  rename(DeathsPer100k_Male_strict = estimate1,
         DeathsPer100k_Male_notstrict = estimate2,
         Difference = estimate)

t_tests <- bind_rows(t_test.all, t_test.firearm, t_test.nonfirearm) 

# Females vs Females across grades

## All suicides ----
differences.all <- deaths.all %>%
  group_by(grade, Age, State) %>%
  filter(is.na(Deaths_Imputed) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(Deaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.all <- broom::tidy(
  t.test(differences.all[differences.all$grade == 'More Strict', ]$DeathsPer100k, 
         differences.all[differences.all$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for ALL Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'All',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)


## Firearm suicides ----
differences.firearm <- deaths.firearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(FirearmDeaths_Imputed) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(FirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 


t_test.firearm <- broom::tidy(
  t.test(differences.firearm[differences.firearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.firearm[differences.firearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for Firearm Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'Firearm',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)


## NonFirearm suicides
differences.nonfirearm <- deaths.nonfirearm %>%
  group_by(grade, Age, State) %>%
  filter(is.na(NonFirearmDeaths_Imputed) == F, Sex == 'Females') %>%
  summarize(DeathsPer100k = sum(NonFirearmDeaths_Imputed) / 
              sum(Population) * 100000) %>% ungroup() 

t_test.nonfirearm <- broom::tidy(
  t.test(differences.nonfirearm[differences.nonfirearm$grade == 'More Strict', ]$DeathsPer100k, 
         differences.nonfirearm[differences.nonfirearm$grade == 'Less Strict', ]$DeathsPer100k)) %>% 
  mutate(
    description = "T-test for NonFirearm Suicides Per 100k between Females and Females in More and Less Strict States",
    imputed_missing = "Yes",
    type = 'NonFirearm',
    sex = 'Females'
  ) %>%
  rename(DeathsPer100k_Female_strict = estimate1,
         DeathsPer100k_Female_notstrict = estimate2,
         Difference = estimate)

t_tests <- bind_rows(t_tests, t_test.suppressed,
                    t_test.all, t_test.firearm, t_test.nonfirearm) 

t_tests <- t_tests %>%
  select(Difference, DeathsPer100k_Male_strict, DeathsPer100k_Male_notstrict,
         DeathsPer100k_Female_strict, DeathsPer100k_Female_notstrict,
         statistic, p.value, parameter, conf.low, conf.high, method, alternative,
         description, imputed_missing, type, sex)

write.csv(t_tests, "outputs/tables/comparisons_within_sex_2015-2021_by_grade.csv", 
          row.names = FALSE)
