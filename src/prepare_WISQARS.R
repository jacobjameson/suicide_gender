################################################################################
# AUTHOR: J. Jameson

# PURPOSE: Prepare the WISQARS data
################################################################################
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 
library(stringr) # for string manipulation)

#-------------------------------------------------------------------------------

# Set the directory path for input and output ----------------------------------
input_path <- "raw/WISQARS"
output_path <- "outputs/clean data/WISQARS"

# List all files in the input directory ----------------------------------------
files <- list.files(input_path, full.names = TRUE)

# Process each file ------------------------------------------------------------
WISQARS <- data.frame()

for (file_name in files) {
  
  # Read the CSV file
  data <- read_csv(file_name)
  
  # Convert 'Age' to numeric
  data$Age <- as.numeric(data$Age)
  
  # Remove commas and convert 'Deaths' and 'Population' to numeric
  data$Deaths <- as.numeric(gsub(",", "", data$Deaths))
  data$Population <- as.numeric(gsub(",", "", data$Population))
  
  # Filter out rows with missing 'Sex' and 'Age' less than 13, rename 'Crude Rate'
  data <- data %>%
    filter(!is.na(Sex), Age >= 13) %>%
    rename(CR = `Crude Rate`)
  
  # Convert 'CR' back to numeric
  data$CR <- as.numeric(data$CR)
  
  # Extract 'Year Group' and 'Mechanism' from the file name
  year_group <- str_extract(basename(file_name), "^\\d+-\\d+")
  mechanism <- str_extract(basename(file_name), "Firearm|NonFirearm")
  
  # Add new columns for 'Year Group' and 'Mechanism'
  data$YearGroup <- year_group
  data$Mechanism <- mechanism
  data$Mechanism <- ifelse(data$Mechanism == 'NonFirearm', 
                           'Non-Firearm', data$Mechanism)
  
  WISQARS <- rbind(WISQARS, data)
}

# Drop columns 'Age-Adjusted Rate' and 'Years of Potential Life Lost' ----------
WISQARS <- WISQARS %>%
  select(-c('Age-Adjusted Rate', 'Years of Potential Life Lost'))


# Add a 'Total' row for each 'Year Group', 'Age', and 'Sex' group --------------
total_deaths <- WISQARS %>%
  group_by(YearGroup, Age, Sex) %>%
  summarize(Deaths = sum(Deaths),
            Population = mean(Population),
            CR = sum(CR),
            Mechanism = "Total",
            .groups = 'drop')

# Append the 'Total' row to the WISQARS data frame -----------------------------
WISQARS <- bind_rows(WISQARS, total_deaths)

WISQARS$YearGroup <- factor(WISQARS$YearGroup, 
                            levels = c("2015-2022", "2008-2014", "2001-2007"))

WISQARS$Mechanism <- factor(WISQARS$Mechanism, 
                            levels = c("Non-Firearm", "Firearm", "Total"))

WISQARS$Sex <- ifelse(WISQARS$Sex == 'Males', 'Male', 'Female')
WISQARS$Sex <- factor(WISQARS$Sex, levels = c('Male', 'Female'))

# Save the processed data as an RDS file ---------------------------------------
saveRDS(WISQARS, file = file.path(output_path, "WISQARS.rds"))

################################################################################