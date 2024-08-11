################################################################################
# AUTHOR: J. Jameson

# PURPOSE: Prepare the WONDER data
################################################################################
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 

#-------------------------------------------------------------------------------

# Set the directory path for input and output ----------------------------------
input_path <- "raw/WONDER"
output_path <- "outputs/clean data/WONDER"

# List all files in the input directory ----------------------------------------
files <- list.files(input_path, full.names = TRUE)


# Define the function to clean the data
clean_data <- function(file_path) {
  
  suppressWarnings(
    data <- read_delim(file_path, delim = "\t", col_names = TRUE)
  )
  
  suppressWarnings(
    data[5:11] <- lapply(data[5:11], function(x) as.numeric(as.character(x)))
  )
  
  data <- data %>% 
    filter(Population != 'Not Applicable', is.na(Gender) == F,
           is.na(`Single-Year Ages Code`) == F) %>%
    select(-Notes) %>% 
    mutate(d = file_path)
  
  return(data)
}

WONDER <- lapply(files, clean_data) %>% bind_rows()

WONDER$mech <- ifelse(grepl('Non-Firearm', WONDER$d), 'Non-Firearm', 'Firearm')
WONDER$grade <- ifelse(grepl('Strict', WONDER$d), 'Strict', 'Permissive')

WONDER <- WONDER %>%
  rename(
    gender = Gender,
    gender_code = `Gender Code`,
    age = `Single-Year Ages`,
    age_code = `Single-Year Ages Code`,
    deaths = Deaths,
    population = Population,
    crude_rate = `Crude Rate`,
    crude_rate_lower_ci = `Crude Rate Lower 95% Confidence Interval`,
    crude_rate_upper_ci = `Crude Rate Upper 95% Confidence Interval`,
    crude_rate_se = `Crude Rate Standard Error`
  ) %>%
  select(-d) %>%
  mutate(crude_rate = deaths/population * 100000)

WONDER %>%
  group_by(grade, age_code, gender) %>%
  summarize(crude_rate = sum(crude_rate, na.rm = T),
            crude_rate_lower_ci = sum(crude_rate_lower_ci, na.rm = T),
            crude_rate_upper_ci = sum(crude_rate_upper_ci, na.rm = T),
            mech = 'Total') %>% ungroup() %>%
  arrange(age_code) %>%
  bind_rows(WONDER) -> WONDER

WONDER$mech <- factor(WONDER$mech, 
                            levels = c("Non-Firearm", "Firearm", "Total"))

WONDER$gender <- factor(WONDER$gender, 
                      levels = c("Male", "Female"))

# Save the processed data as an RDS file ---------------------------------------
saveRDS(WONDER, file = file.path(output_path, "WONDER.rds"))

################################################################################