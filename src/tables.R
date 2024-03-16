################################################################################
# AUTHOR: J. Jameson

################################################################################

# Load libraries --------------------------------------------------------------
library(tidyverse) # for data manipulation 
library(ggthemes) # for additional themes
library(RColorBrewer) # for color palettes

# Load data --------------------------------------------------------------------
avg_deaths <- read_csv("outputs/data/deaths_cleaned_age.csv")
