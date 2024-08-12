################################################################################
# AUTHOR: J. Jameson

# PURPOSE: Create tables for the manuscript
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Load data -------------------------------------------------------------------
WONDER <- readRDS("outputs/clean data/WONDER/WONDER.rds")

# Create age group variable ---------------------------------------------------
WONDER$age_group <- cut(WONDER$age_code, breaks=seq(0, 100, by=5), right=FALSE,
                      labels=paste(seq(0, 95, by=5), seq(4, 99, by=5), sep="-"))


# Run ANOVA and get a summary -------------------------------------------------- 
anova_result <- aov(crude_rate ~ age_group * gender * mech * grade, 
                    data = WONDER)

summary(anova_result)

# Post-hoc test using Tukey's HSD
tables <- TukeyHSD(anova_result) %>% broom::tidy() %>% 
  mutate(significant = adj.p.value < 0.05) 

################################################################################
# Table 1. Suicides, Stratified by Sex -----------------------------------------
################################################################################

groups <- c('10-14:Female:Firearm-10-14:Male:Firearm',
            '15-19:Female:Firearm-15-19:Male:Firearm',
            '20-24:Female:Firearm-20-24:Male:Firearm',
            '25-29:Female:Firearm-25-29:Male:Firearm',
            '30-34:Female:Firearm-30-34:Male:Firearm',
            '35-39:Female:Firearm-35-39:Male:Firearm',
            '40-44:Female:Firearm-40-44:Male:Firearm',
            '45-49:Female:Firearm-45-49:Male:Firearm',
            '50-54:Female:Firearm-50-54:Male:Firearm',
            '55-59:Female:Firearm-55-59:Male:Firearm',
            '60-64:Female:Firearm-60-64:Male:Firearm',
            '65-69:Female:Firearm-65-69:Male:Firearm',
            '70-74:Female:Firearm-70-74:Male:Firearm',
            '75-79:Female:Firearm-75-79:Male:Firearm',
            '80-84:Female:Firearm-80-84:Male:Firearm',
            
            '10-14:Female:Non-Firearm-10-14:Male:Non-Firearm',
            '15-19:Female:Non-Firearm-15-19:Male:Non-Firearm',
            '20-24:Female:Non-Firearm-20-24:Male:Non-Firearm',
            '25-29:Female:Non-Firearm-25-29:Male:Non-Firearm',
            '30-34:Female:Non-Firearm-30-34:Male:Non-Firearm',
            '35-39:Female:Non-Firearm-35-39:Male:Non-Firearm',
            '40-44:Female:Non-Firearm-40-44:Male:Non-Firearm',
            '45-49:Female:Non-Firearm-45-49:Male:Non-Firearm',
            '50-54:Female:Non-Firearm-50-54:Male:Non-Firearm',
            '55-59:Female:Non-Firearm-55-59:Male:Non-Firearm',
            '60-64:Female:Non-Firearm-60-64:Male:Non-Firearm',
            '65-69:Female:Non-Firearm-65-69:Male:Non-Firearm',
            '70-74:Female:Non-Firearm-70-74:Male:Non-Firearm',
            '75-79:Female:Non-Firearm-75-79:Male:Non-Firearm',
            '80-84:Female:Non-Firearm-80-84:Male:Non-Firearm',
            
            '10-14:Female:Total-10-14:Male:Total',
            '15-19:Female:Total-15-19:Male:Total',
            '20-24:Female:Total-20-24:Male:Total',
            '25-29:Female:Total-25-29:Male:Total',
            '30-34:Female:Total-30-34:Male:Total',
            '35-39:Female:Total-35-39:Male:Total',
            '40-44:Female:Total-40-44:Male:Total',
            '45-49:Female:Total-45-49:Male:Total',
            '50-54:Female:Total-50-54:Male:Total',
            '55-59:Female:Total-55-59:Male:Total',
            '60-64:Female:Total-60-64:Male:Total',
            '65-69:Female:Total-65-69:Male:Total',
            '70-74:Female:Total-70-74:Male:Total',
            '75-79:Female:Total-75-79:Male:Total',
            '80-84:Female:Total-80-84:Male:Total')

t1 <- tables %>% 
  filter(term == 'age_group:gender:mech', contrast %in% groups) %>%
  cbind(
    WONDER %>%
      group_by(age_group, gender, mech) %>%
      summarise(crude_rate = mean(crude_rate, na.rm = T)) %>%
      pivot_wider(names_from = gender, 
                  values_from = crude_rate, 
                  names_prefix = "crude_rate_") %>%
      filter(age_group != '0-4', age_group != '5-9') %>%
      ungroup() %>%
      arrange(mech, age_group)) %>%
  mutate(
    age_group = str_extract(contrast, "^\\d+-\\d+"),
    mechanism = str_extract(contrast, "(Firearm|Non-Firearm|Total)")
  ) %>%
  select(age_group, mechanism, crude_rate_Female, crude_rate_Male,
                difference = estimate, conf.low, conf.high, significant,
                adj.p.value) %>%
  mutate_if(is.numeric, round, 3) 

# format table 1 ---------------------------------------------------------------
t1 <- t1 %>%
  select(age_group, mechanism, crude_rate_Male, crude_rate_Female, 
         difference, adj.p.value) %>%
  pivot_wider(
    names_from = mechanism,
    values_from = c(crude_rate_Male, 
                    crude_rate_Female, 
                    difference,
                    adj.p.value)
  ) %>%
  rename(
    Male_Total = crude_rate_Male_Total,
    Female_Total = crude_rate_Female_Total,
    Difference_Total = difference_Total,
    p_total = adj.p.value_Total,
    Male_Firearm = crude_rate_Male_Firearm,
    Female_Firearm = crude_rate_Female_Firearm,
    Difference_Firearm = difference_Firearm,
    p_firearm = adj.p.value_Firearm,
    Male_NonFirearm = `crude_rate_Male_Non-Firearm`,
    Female_NonFirearm = `crude_rate_Female_Non-Firearm`,
    Difference_NonFirearm = `difference_Non-Firearm`,
    p_nonfirearm = `adj.p.value_Non-Firearm`
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(p_total = ifelse(p_total < 0.001, "<0.001", p_total),
         p_firearm = ifelse(p_firearm < 0.001, "<0.001", p_firearm),
         p_nonfirearm = ifelse(p_nonfirearm < 0.001, "<0.001", p_nonfirearm),
         Difference_Total = Difference_Total * -1,
         Difference_Firearm = Difference_Firearm * -1,
         Difference_NonFirearm = Difference_NonFirearm * -1) %>%
  select(age_group,
         Male_Total, Female_Total, Difference_Total, p_total,
         Male_Firearm, Female_Firearm, Difference_Firearm, p_firearm,
         Male_NonFirearm, Female_NonFirearm, Difference_NonFirearm,
         p_nonfirearm) %>%
  kable(caption = "Table 1. Suicides rates per 100,000, Stratified by Sex",
      align = c("l", rep("r", 12)),
      col.names = c("Age Group", 
                    "Male", "Female", "Difference", "p-value",
                    "Male", "Female", "Difference", "p-value",
                    "Male", "Female", "Difference", 'p-value')) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Total" = 4, "Firearm" = 4, "Non-Firearm" = 4))

save_kable(t1, file = "outputs/tables/Table1.pdf")


################################################################################
# Table 2. Male Suicides, Stratified by State Policy Environment ---------------
################################################################################

groups <- c('10-14:Male:Firearm:Strict-10-14:Male:Firearm:Permissive',
            '15-19:Male:Firearm:Strict-15-19:Male:Firearm:Permissive',
            '20-24:Male:Firearm:Strict-20-24:Male:Firearm:Permissive',
            '25-29:Male:Firearm:Strict-25-29:Male:Firearm:Permissive',
            '30-34:Male:Firearm:Strict-30-34:Male:Firearm:Permissive',
            '35-39:Male:Firearm:Strict-35-39:Male:Firearm:Permissive',
            '40-44:Male:Firearm:Strict-40-44:Male:Firearm:Permissive',
            '45-49:Male:Firearm:Strict-45-49:Male:Firearm:Permissive',
            '50-54:Male:Firearm:Strict-50-54:Male:Firearm:Permissive',
            '55-59:Male:Firearm:Strict-55-59:Male:Firearm:Permissive',
            '60-64:Male:Firearm:Strict-60-64:Male:Firearm:Permissive',
            '65-69:Male:Firearm:Strict-65-69:Male:Firearm:Permissive',
            '70-74:Male:Firearm:Strict-70-74:Male:Firearm:Permissive',
            '75-79:Male:Firearm:Strict-75-79:Male:Firearm:Permissive',
            '80-84:Male:Firearm:Strict-80-84:Male:Firearm:Permissive',
            
            '10-14:Male:Non-Firearm:Strict-10-14:Male:Non-Firearm:Permissive',
            '15-19:Male:Non-Firearm:Strict-15-19:Male:Non-Firearm:Permissive',
            '20-24:Male:Non-Firearm:Strict-20-24:Male:Non-Firearm:Permissive',
            '25-29:Male:Non-Firearm:Strict-25-29:Male:Non-Firearm:Permissive',
            '30-34:Male:Non-Firearm:Strict-30-34:Male:Non-Firearm:Permissive',
            '35-39:Male:Non-Firearm:Strict-35-39:Male:Non-Firearm:Permissive',
            '40-44:Male:Non-Firearm:Strict-40-44:Male:Non-Firearm:Permissive',
            '45-49:Male:Non-Firearm:Strict-45-49:Male:Non-Firearm:Permissive',
            '50-54:Male:Non-Firearm:Strict-50-54:Male:Non-Firearm:Permissive',
            '55-59:Male:Non-Firearm:Strict-55-59:Male:Non-Firearm:Permissive',
            '60-64:Male:Non-Firearm:Strict-60-64:Male:Non-Firearm:Permissive',
            '65-69:Male:Non-Firearm:Strict-65-69:Male:Non-Firearm:Permissive',
            '70-74:Male:Non-Firearm:Strict-70-74:Male:Non-Firearm:Permissive',
            '75-79:Male:Non-Firearm:Strict-75-79:Male:Non-Firearm:Permissive',
            '80-84:Male:Non-Firearm:Strict-80-84:Male:Non-Firearm:Permissive',
            
            '10-14:Male:Total:Strict-10-14:Male:Total:Permissive',
            '15-19:Male:Total:Strict-15-19:Male:Total:Permissive',
            '20-24:Male:Total:Strict-20-24:Male:Total:Permissive',
            '25-29:Male:Total:Strict-25-29:Male:Total:Permissive',
            '30-34:Male:Total:Strict-30-34:Male:Total:Permissive',
            '35-39:Male:Total:Strict-35-39:Male:Total:Permissive',
            '40-44:Male:Total:Strict-40-44:Male:Total:Permissive',
            '45-49:Male:Total:Strict-45-49:Male:Total:Permissive',
            '50-54:Male:Total:Strict-50-54:Male:Total:Permissive',
            '55-59:Male:Total:Strict-55-59:Male:Total:Permissive',
            '60-64:Male:Total:Strict-60-64:Male:Total:Permissive',
            '65-69:Male:Total:Strict-65-69:Male:Total:Permissive',
            '70-74:Male:Total:Strict-70-74:Male:Total:Permissive',
            '75-79:Male:Total:Strict-75-79:Male:Total:Permissive',
            '80-84:Male:Total:Strict-80-84:Male:Total:Permissive')


t2 <- tables %>% 
  filter(term == 'age_group:gender:mech:grade', contrast %in% groups) %>%
  cbind(
    WONDER %>%
      filter(gender == 'Male') %>%
      group_by(age_group, mech, grade) %>%
      summarise(crude_rate = mean(crude_rate, na.rm = T)) %>%
      pivot_wider(names_from = grade, 
                  values_from = crude_rate,
                  names_prefix = "crude_rate_") %>%
      filter(age_group != '0-4', age_group != '5-9') %>% 
      ungroup() %>%
      arrange(mech, age_group))  %>%
  mutate(
    age_group = str_extract(contrast, "^\\d+-\\d+"),
    mechanism = str_extract(contrast, "(Firearm|Non-Firearm|Total)")
  ) %>%
  dplyr::select(age_group, mechanism, crude_rate_Permissive, crude_rate_Strict,
                difference = estimate, conf.low, conf.high, significant,
                adj.p.value) %>%
  mutate_if(is.numeric, round, 3) 

# format table 2 ---------------------------------------------------------------
t2 <- t2 %>%
  select(age_group, mechanism, crude_rate_Permissive, crude_rate_Strict, 
         difference, adj.p.value) %>%
  pivot_wider(
    names_from = mechanism,
    values_from = c(crude_rate_Permissive, 
                    crude_rate_Strict, 
                    difference,
                    adj.p.value)
  ) %>%
  rename(
    Permissive_Total = crude_rate_Permissive_Total,
    Strict_Total = crude_rate_Strict_Total,
    Difference_Total = difference_Total,
    p_total = adj.p.value_Total,
    Permissive_Firearm = crude_rate_Permissive_Firearm,
    Strict_Firearm = crude_rate_Strict_Firearm,
    Difference_Firearm = difference_Firearm,
    p_firearm = adj.p.value_Firearm,
    Permissive_NonFirearm = `crude_rate_Permissive_Non-Firearm`,
    Strict_NonFirearm = `crude_rate_Strict_Non-Firearm`,
    Difference_NonFirearm = `difference_Non-Firearm`,
    p_nonfirearm = `adj.p.value_Non-Firearm`
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(p_total = ifelse(p_total < 0.001, "<0.001", p_total),
         p_firearm = ifelse(p_firearm < 0.001, "<0.001", p_firearm),
         p_nonfirearm = ifelse(p_nonfirearm < 0.001, "<0.001", p_nonfirearm),
         Difference_Total = Difference_Total * -1,
         Difference_Firearm = Difference_Firearm * -1,
         Difference_NonFirearm = Difference_NonFirearm * -1) %>%
  select(age_group,
         Strict_Total, Permissive_Total, Difference_Total, p_total,
         Strict_Firearm, Permissive_Firearm, Difference_Firearm, p_firearm,
         Strict_NonFirearm, Permissive_NonFirearm, Difference_NonFirearm,
         p_nonfirearm) %>%
  kable(caption = "Table 2. Male Suicides per 100,000, Stratified by State Policy Environment",
        align = c("l", rep("r", 12)),
        col.names = c("Age Group", 
                      "Male Strict", "Male Permissive", "Difference", "p-value",
                      "Male Strict", "Male Permissive", "Difference", "p-value",
                      "Male Strict", "Male Permissive", "Difference", 'p-value')) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Total" = 4, "Firearm" = 4, "Non-Firearm" = 4))

save_kable(t2, file = "outputs/tables/Table2.pdf")

################################################################################
# Table 3
################################################################################

groups <- c('10-14:Female:Firearm:Strict-10-14:Female:Firearm:Permissive',
            '15-19:Female:Firearm:Strict-15-19:Female:Firearm:Permissive',
            '20-24:Female:Firearm:Strict-20-24:Female:Firearm:Permissive',
            '25-29:Female:Firearm:Strict-25-29:Female:Firearm:Permissive',
            '30-34:Female:Firearm:Strict-30-34:Female:Firearm:Permissive',
            '35-39:Female:Firearm:Strict-35-39:Female:Firearm:Permissive',
            '40-44:Female:Firearm:Strict-40-44:Female:Firearm:Permissive',
            '45-49:Female:Firearm:Strict-45-49:Female:Firearm:Permissive',
            '50-54:Female:Firearm:Strict-50-54:Female:Firearm:Permissive',
            '55-59:Female:Firearm:Strict-55-59:Female:Firearm:Permissive',
            '60-64:Female:Firearm:Strict-60-64:Female:Firearm:Permissive',
            '65-69:Female:Firearm:Strict-65-69:Female:Firearm:Permissive',
            '70-74:Female:Firearm:Strict-70-74:Female:Firearm:Permissive',
            '75-79:Female:Firearm:Strict-75-79:Female:Firearm:Permissive',
            '80-84:Female:Firearm:Strict-80-84:Female:Firearm:Permissive',
            
            '10-14:Female:Non-Firearm:Strict-10-14:Female:Non-Firearm:Permissive',
            '15-19:Female:Non-Firearm:Strict-15-19:Female:Non-Firearm:Permissive',
            '20-24:Female:Non-Firearm:Strict-20-24:Female:Non-Firearm:Permissive',
            '25-29:Female:Non-Firearm:Strict-25-29:Female:Non-Firearm:Permissive',
            '30-34:Female:Non-Firearm:Strict-30-34:Female:Non-Firearm:Permissive',
            '35-39:Female:Non-Firearm:Strict-35-39:Female:Non-Firearm:Permissive',
            '40-44:Female:Non-Firearm:Strict-40-44:Female:Non-Firearm:Permissive',
            '45-49:Female:Non-Firearm:Strict-45-49:Female:Non-Firearm:Permissive',
            '50-54:Female:Non-Firearm:Strict-50-54:Female:Non-Firearm:Permissive',
            '55-59:Female:Non-Firearm:Strict-55-59:Female:Non-Firearm:Permissive',
            '60-64:Female:Non-Firearm:Strict-60-64:Female:Non-Firearm:Permissive',
            '65-69:Female:Non-Firearm:Strict-65-69:Female:Non-Firearm:Permissive',
            '70-74:Female:Non-Firearm:Strict-70-74:Female:Non-Firearm:Permissive',
            '75-79:Female:Non-Firearm:Strict-75-79:Female:Non-Firearm:Permissive',
            '80-84:Female:Non-Firearm:Strict-80-84:Female:Non-Firearm:Permissive',
            
            '10-14:Female:Total:Strict-10-14:Female:Total:Permissive',
            '15-19:Female:Total:Strict-15-19:Female:Total:Permissive',
            '20-24:Female:Total:Strict-20-24:Female:Total:Permissive',
            '25-29:Female:Total:Strict-25-29:Female:Total:Permissive',
            '30-34:Female:Total:Strict-30-34:Female:Total:Permissive',
            '35-39:Female:Total:Strict-35-39:Female:Total:Permissive',
            '40-44:Female:Total:Strict-40-44:Female:Total:Permissive',
            '45-49:Female:Total:Strict-45-49:Female:Total:Permissive',
            '50-54:Female:Total:Strict-50-54:Female:Total:Permissive',
            '55-59:Female:Total:Strict-55-59:Female:Total:Permissive',
            '60-64:Female:Total:Strict-60-64:Female:Total:Permissive',
            '65-69:Female:Total:Strict-65-69:Female:Total:Permissive',
            '70-74:Female:Total:Strict-70-74:Female:Total:Permissive',
            '75-79:Female:Total:Strict-75-79:Female:Total:Permissive',
            '80-84:Female:Total:Strict-80-84:Female:Total:Permissive')


t3 <- tables %>% 
  filter(term == 'age_group:gender:mech:grade', contrast %in% groups) %>%
  cbind(
    WONDER %>%
      filter(gender == 'Female') %>%
      group_by(age_group, mech, grade) %>%
      summarise(crude_rate = mean(crude_rate, na.rm = T)) %>%
      pivot_wider(names_from = grade, 
                  values_from = crude_rate,
                  names_prefix = "crude_rate_") %>%
      filter(age_group != '0-4', age_group != '5-9') %>% 
      ungroup() %>%
      arrange(mech, age_group))  %>%
  mutate(
    age_group = str_extract(contrast, "^\\d+-\\d+"),
    mechanism = str_extract(contrast, "(Firearm|Non-Firearm|Total)")
  ) %>%
  dplyr::select(age_group, mechanism, crude_rate_Permissive, crude_rate_Strict,
                difference = estimate, conf.low, conf.high, significant,
                adj.p.value) %>%
  mutate_if(is.numeric, round, 3) 

# format table 3 ---------------------------------------------------------------
t3 <- t3 %>%
  select(age_group, mechanism, crude_rate_Permissive, crude_rate_Strict, 
         difference, adj.p.value) %>%
  pivot_wider(
    names_from = mechanism,
    values_from = c(crude_rate_Permissive, 
                    crude_rate_Strict, 
                    difference,
                    adj.p.value)
  ) %>%
  rename(
    Permissive_Total = crude_rate_Permissive_Total,
    Strict_Total = crude_rate_Strict_Total,
    Difference_Total = difference_Total,
    p_total = adj.p.value_Total,
    Permissive_Firearm = crude_rate_Permissive_Firearm,
    Strict_Firearm = crude_rate_Strict_Firearm,
    Difference_Firearm = difference_Firearm,
    p_firearm = adj.p.value_Firearm,
    Permissive_NonFirearm = `crude_rate_Permissive_Non-Firearm`,
    Strict_NonFirearm = `crude_rate_Strict_Non-Firearm`,
    Difference_NonFirearm = `difference_Non-Firearm`,
    p_nonfirearm = `adj.p.value_Non-Firearm`
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(p_total = ifelse(p_total < 0.001, "<0.001", p_total),
         p_firearm = ifelse(p_firearm < 0.001, "<0.001", p_firearm),
         p_nonfirearm = ifelse(p_nonfirearm < 0.001, "<0.001", p_nonfirearm),
         Difference_Total = Difference_Total * -1,
         Difference_Firearm = Difference_Firearm * -1,
         Difference_NonFirearm = Difference_NonFirearm * -1) %>%
  select(age_group,
         Strict_Total, Permissive_Total, Difference_Total, p_total,
         Strict_Firearm, Permissive_Firearm, Difference_Firearm, p_firearm,
         Strict_NonFirearm, Permissive_NonFirearm, Difference_NonFirearm,
         p_nonfirearm) %>%
  kable(caption = "Table 3. Female Suicides per 100,000, Stratified by State Policy Environment",
        align = c("l", rep("r", 12)),
        col.names = c("Age Group", 
                      "Female Strict", "Female Permissive", "Difference", "p-value",
                      "Female Strict", "Female Permissive", "Difference", "p-value",
                      "Female Strict", "Female Permissive", "Difference", 'p-value')) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Total" = 4, "Firearm" = 4, "Non-Firearm" = 4))

save_kable(t3, file = "outputs/tables/Table3.pdf")

################################################################################
# Table 4
################################################################################

groups <- c('10-14:Female:Firearm:Permissive-10-14:Male:Firearm:Permissive',
            '15-19:Female:Firearm:Permissive-15-19:Male:Firearm:Permissive',
            '20-24:Female:Firearm:Permissive-20-24:Male:Firearm:Permissive',
            '25-29:Female:Firearm:Permissive-25-29:Male:Firearm:Permissive',
            '30-34:Female:Firearm:Permissive-30-34:Male:Firearm:Permissive',
            '35-39:Female:Firearm:Permissive-35-39:Male:Firearm:Permissive',
            '40-44:Female:Firearm:Permissive-40-44:Male:Firearm:Permissive',
            '45-49:Female:Firearm:Permissive-45-49:Male:Firearm:Permissive',
            '50-54:Female:Firearm:Permissive-50-54:Male:Firearm:Permissive',
            '55-59:Female:Firearm:Permissive-55-59:Male:Firearm:Permissive',
            '60-64:Female:Firearm:Permissive-60-64:Male:Firearm:Permissive',
            '65-69:Female:Firearm:Permissive-65-69:Male:Firearm:Permissive',
            '70-74:Female:Firearm:Permissive-70-74:Male:Firearm:Permissive',
            '75-79:Female:Firearm:Permissive-75-79:Male:Firearm:Permissive',
            '80-84:Female:Firearm:Permissive-80-84:Male:Firearm:Permissive',
            
            '10-14:Female:Non-Firearm:Permissive-10-14:Male:Non-Firearm:Permissive',
            '15-19:Female:Non-Firearm:Permissive-15-19:Male:Non-Firearm:Permissive',
            '20-24:Female:Non-Firearm:Permissive-20-24:Male:Non-Firearm:Permissive',
            '25-29:Female:Non-Firearm:Permissive-25-29:Male:Non-Firearm:Permissive',
            '30-34:Female:Non-Firearm:Permissive-30-34:Male:Non-Firearm:Permissive',
            '35-39:Female:Non-Firearm:Permissive-35-39:Male:Non-Firearm:Permissive',
            '40-44:Female:Non-Firearm:Permissive-40-44:Male:Non-Firearm:Permissive',
            '45-49:Female:Non-Firearm:Permissive-45-49:Male:Non-Firearm:Permissive',
            '50-54:Female:Non-Firearm:Permissive-50-54:Male:Non-Firearm:Permissive',
            '55-59:Female:Non-Firearm:Permissive-55-59:Male:Non-Firearm:Permissive',
            '60-64:Female:Non-Firearm:Permissive-60-64:Male:Non-Firearm:Permissive',
            '65-69:Female:Non-Firearm:Permissive-65-69:Male:Non-Firearm:Permissive',
            '70-74:Female:Non-Firearm:Permissive-70-74:Male:Non-Firearm:Permissive',
            '75-79:Female:Non-Firearm:Permissive-75-79:Male:Non-Firearm:Permissive',
            '80-84:Female:Non-Firearm:Permissive-80-84:Male:Non-Firearm:Permissive',
            
            '10-14:Female:Total:Permissive-10-14:Male:Total:Permissive',
            '15-19:Female:Total:Permissive-15-19:Male:Total:Permissive',
            '20-24:Female:Total:Permissive-20-24:Male:Total:Permissive',
            '25-29:Female:Total:Permissive-25-29:Male:Total:Permissive',
            '30-34:Female:Total:Permissive-30-34:Male:Total:Permissive',
            '35-39:Female:Total:Permissive-35-39:Male:Total:Permissive',
            '40-44:Female:Total:Permissive-40-44:Male:Total:Permissive',
            '45-49:Female:Total:Permissive-45-49:Male:Total:Permissive',
            '50-54:Female:Total:Permissive-50-54:Male:Total:Permissive',
            '55-59:Female:Total:Permissive-55-59:Male:Total:Permissive',
            '60-64:Female:Total:Permissive-60-64:Male:Total:Permissive',
            '65-69:Female:Total:Permissive-65-69:Male:Total:Permissive',
            '70-74:Female:Total:Permissive-70-74:Male:Total:Permissive',
            '75-79:Female:Total:Permissive-75-79:Male:Total:Permissive',
            '80-84:Female:Total:Permissive-80-84:Male:Total:Permissive')


t4 <- tables %>% 
  filter(term == 'age_group:gender:mech:grade', contrast %in% groups) %>%
  cbind(
    WONDER %>%
      filter(grade == 'Permissive') %>%
      group_by(age_group, mech, gender) %>%
      summarise(crude_rate = mean(crude_rate, na.rm = T)) %>%
      pivot_wider(names_from = gender, 
                  values_from = crude_rate,
                  names_prefix = "crude_rate_") %>%
      filter(age_group != '0-4', age_group != '5-9') %>% 
      ungroup() %>%
      arrange(mech, age_group))  %>%
  mutate(
    age_group = str_extract(contrast, "^\\d+-\\d+"),
    mechanism = str_extract(contrast, "(Firearm|Non-Firearm|Total)")
  ) %>%
  select(age_group, mechanism, crude_rate_Female, crude_rate_Male,
         difference = estimate, conf.low, conf.high, significant,
         adj.p.value) %>%
  mutate_if(is.numeric, round, 3) 


# format table 4 ---------------------------------------------------------------
t4 <- t4 %>%
  select(age_group, mechanism, crude_rate_Male, crude_rate_Female, 
         difference, adj.p.value) %>%
  pivot_wider(
    names_from = mechanism,
    values_from = c(crude_rate_Male, 
                    crude_rate_Female, 
                    difference,
                    adj.p.value)
  ) %>%
  rename(
    Male_Total = crude_rate_Male_Total,
    Female_Total = crude_rate_Female_Total,
    Difference_Total = difference_Total,
    p_total = adj.p.value_Total,
    Male_Firearm = crude_rate_Male_Firearm,
    Female_Firearm = crude_rate_Female_Firearm,
    Difference_Firearm = difference_Firearm,
    p_firearm = adj.p.value_Firearm,
    Male_NonFirearm = `crude_rate_Male_Non-Firearm`,
    Female_NonFirearm = `crude_rate_Female_Non-Firearm`,
    Difference_NonFirearm = `difference_Non-Firearm`,
    p_nonfirearm = `adj.p.value_Non-Firearm`
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(p_total = ifelse(p_total < 0.001, "<0.001", p_total),
         p_firearm = ifelse(p_firearm < 0.001, "<0.001", p_firearm),
         p_nonfirearm = ifelse(p_nonfirearm < 0.001, "<0.001", p_nonfirearm),
         Difference_Total = Difference_Total * -1,
         Difference_Firearm = Difference_Firearm * -1,
         Difference_NonFirearm = Difference_NonFirearm * -1) %>%
  select(age_group,
         Male_Total, Female_Total, Difference_Total, p_total,
         Male_Firearm, Female_Firearm, Difference_Firearm, p_firearm,
         Male_NonFirearm, Female_NonFirearm, Difference_NonFirearm,
         p_nonfirearm) %>%
  kable(caption = "Table 4. Suicides per 100,000, Stratified by Sex, within Permissive Policy Environment",
        align = c("l", rep("r", 12)),
        col.names = c("Age Group", 
                      "Male", "Female", "Difference", "p-value",
                      "Male", "Female", "Difference", "p-value",
                      "Male", "Female", "Difference", 'p-value')) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Total" = 4, "Firearm" = 4, "Non-Firearm" = 4))

save_kable(t4, file = "outputs/tables/Table4.pdf")

################################################################################
# Table 5
################################################################################

groups <- c('10-14:Female:Firearm:Strict-10-14:Male:Firearm:Strict',
            '15-19:Female:Firearm:Strict-15-19:Male:Firearm:Strict',
            '20-24:Female:Firearm:Strict-20-24:Male:Firearm:Strict',
            '25-29:Female:Firearm:Strict-25-29:Male:Firearm:Strict',
            '30-34:Female:Firearm:Strict-30-34:Male:Firearm:Strict',
            '35-39:Female:Firearm:Strict-35-39:Male:Firearm:Strict',
            '40-44:Female:Firearm:Strict-40-44:Male:Firearm:Strict',
            '45-49:Female:Firearm:Strict-45-49:Male:Firearm:Strict',
            '50-54:Female:Firearm:Strict-50-54:Male:Firearm:Strict',
            '55-59:Female:Firearm:Strict-55-59:Male:Firearm:Strict',
            '60-64:Female:Firearm:Strict-60-64:Male:Firearm:Strict',
            '65-69:Female:Firearm:Strict-65-69:Male:Firearm:Strict',
            '70-74:Female:Firearm:Strict-70-74:Male:Firearm:Strict',
            '75-79:Female:Firearm:Strict-75-79:Male:Firearm:Strict',
            '80-84:Female:Firearm:Strict-80-84:Male:Firearm:Strict',
            
            '10-14:Female:Non-Firearm:Strict-10-14:Male:Non-Firearm:Strict',
            '15-19:Female:Non-Firearm:Strict-15-19:Male:Non-Firearm:Strict',
            '20-24:Female:Non-Firearm:Strict-20-24:Male:Non-Firearm:Strict',
            '25-29:Female:Non-Firearm:Strict-25-29:Male:Non-Firearm:Strict',
            '30-34:Female:Non-Firearm:Strict-30-34:Male:Non-Firearm:Strict',
            '35-39:Female:Non-Firearm:Strict-35-39:Male:Non-Firearm:Strict',
            '40-44:Female:Non-Firearm:Strict-40-44:Male:Non-Firearm:Strict',
            '45-49:Female:Non-Firearm:Strict-45-49:Male:Non-Firearm:Strict',
            '50-54:Female:Non-Firearm:Strict-50-54:Male:Non-Firearm:Strict',
            '55-59:Female:Non-Firearm:Strict-55-59:Male:Non-Firearm:Strict',
            '60-64:Female:Non-Firearm:Strict-60-64:Male:Non-Firearm:Strict',
            '65-69:Female:Non-Firearm:Strict-65-69:Male:Non-Firearm:Strict',
            '70-74:Female:Non-Firearm:Strict-70-74:Male:Non-Firearm:Strict',
            '75-79:Female:Non-Firearm:Strict-75-79:Male:Non-Firearm:Strict',
            '80-84:Female:Non-Firearm:Strict-80-84:Male:Non-Firearm:Strict',
            
            '10-14:Female:Total:Strict-10-14:Male:Total:Strict',
            '15-19:Female:Total:Strict-15-19:Male:Total:Strict',
            '20-24:Female:Total:Strict-20-24:Male:Total:Strict',
            '25-29:Female:Total:Strict-25-29:Male:Total:Strict',
            '30-34:Female:Total:Strict-30-34:Male:Total:Strict',
            '35-39:Female:Total:Strict-35-39:Male:Total:Strict',
            '40-44:Female:Total:Strict-40-44:Male:Total:Strict',
            '45-49:Female:Total:Strict-45-49:Male:Total:Strict',
            '50-54:Female:Total:Strict-50-54:Male:Total:Strict',
            '55-59:Female:Total:Strict-55-59:Male:Total:Strict',
            '60-64:Female:Total:Strict-60-64:Male:Total:Strict',
            '65-69:Female:Total:Strict-65-69:Male:Total:Strict',
            '70-74:Female:Total:Strict-70-74:Male:Total:Strict',
            '75-79:Female:Total:Strict-75-79:Male:Total:Strict',
            '80-84:Female:Total:Strict-80-84:Male:Total:Strict')


t5 <- tables %>% 
  filter(term == 'age_group:gender:mech:grade', contrast %in% groups) %>%
  cbind(
    WONDER %>%
      filter(grade == 'Strict') %>%
      group_by(age_group, mech, gender) %>%
      summarise(crude_rate = mean(crude_rate, na.rm = T)) %>%
      pivot_wider(names_from = gender, 
                  values_from = crude_rate,
                  names_prefix = "crude_rate_") %>%
      filter(age_group != '0-4', age_group != '5-9') %>% 
      ungroup() %>%
      arrange(mech, age_group))  %>%
  mutate(
    age_group = str_extract(contrast, "^\\d+-\\d+"),
    mechanism = str_extract(contrast, "(Firearm|Non-Firearm|Total)")
  ) %>%
  select(age_group, mechanism, crude_rate_Female, crude_rate_Male,
         difference = estimate, conf.low, conf.high, significant,
         adj.p.value) %>%
  mutate_if(is.numeric, round, 3) 


# format table 5 ---------------------------------------------------------------
t5 <- t5 %>%
  select(age_group, mechanism, crude_rate_Male, crude_rate_Female, 
         difference, adj.p.value) %>%
  pivot_wider(
    names_from = mechanism,
    values_from = c(crude_rate_Male, 
                    crude_rate_Female, 
                    difference,
                    adj.p.value)
  ) %>%
  rename(
    Male_Total = crude_rate_Male_Total,
    Female_Total = crude_rate_Female_Total,
    Difference_Total = difference_Total,
    p_total = adj.p.value_Total,
    Male_Firearm = crude_rate_Male_Firearm,
    Female_Firearm = crude_rate_Female_Firearm,
    Difference_Firearm = difference_Firearm,
    p_firearm = adj.p.value_Firearm,
    Male_NonFirearm = `crude_rate_Male_Non-Firearm`,
    Female_NonFirearm = `crude_rate_Female_Non-Firearm`,
    Difference_NonFirearm = `difference_Non-Firearm`,
    p_nonfirearm = `adj.p.value_Non-Firearm`
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(p_total = ifelse(p_total < 0.001, "<0.001", p_total),
         p_firearm = ifelse(p_firearm < 0.001, "<0.001", p_firearm),
         p_nonfirearm = ifelse(p_nonfirearm < 0.001, "<0.001", p_nonfirearm),
         Difference_Total = Difference_Total * -1,
         Difference_Firearm = Difference_Firearm * -1,
         Difference_NonFirearm = Difference_NonFirearm * -1) %>%
  select(age_group,
         Male_Total, Female_Total, Difference_Total, p_total,
         Male_Firearm, Female_Firearm, Difference_Firearm, p_firearm,
         Male_NonFirearm, Female_NonFirearm, Difference_NonFirearm,
         p_nonfirearm) %>%
  kable(caption = "Table 5. Suicides per 100,000, Stratified by Sex, within Strict Policy Environment",
        align = c("l", rep("r", 12)),
        col.names = c("Age Group", 
                      "Male", "Female", "Difference", "p-value",
                      "Male", "Female", "Difference", "p-value",
                      "Male", "Female", "Difference", 'p-value')) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Total" = 4, "Firearm" = 4, "Non-Firearm" = 4))

save_kable(t5, file = "outputs/tables/Table5.pdf")

################################################################################