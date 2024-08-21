################################################################################
# AUTHOR: J. Jameson

# PURPOSE: Create figures
################################################################################
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggthemes)

# Define color palettes --------------------------------------------------------
year_colors <- c(
  "2001-2007" = "grey70",
  "2008-2014" = "grey30",
  "2015-2022" = "#B100000"
)

gun_colors <- c(
  "Permissive" = "#780606",
  "Strict" = "grey70"
)

# Define common theme for figures ----------------------------------------------
publication_theme <- function() {
  theme_economist() +
    theme(
      plot.background = element_rect(fill = 'white', color = NA),
      axis.text = element_text(color = 'black', size = 18),
      plot.title = element_text(size = 30, color = 'black'),
      plot.subtitle = element_text(size = 18, hjust = 0),
      panel.grid.major = element_line(color = 'grey85', size = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = 'right',
      legend.title = element_text(size = 18, face = 'bold'),
      legend.text = element_text(size = 18),
      legend.key.size = unit(1.5, "lines"),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(1, "cm"),
      axis.title = element_text(size = 18, color = 'black'),
      strip.text.x = element_text(size = 24, face = "bold", color = 'black',
                                  margin = margin(0.5,0,0.5,0, "cm")),
      strip.background = element_rect(fill = 'grey85', color = 'white'),
      strip.placement = "outside",
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(fill = "white")
    )
}

# Function to create Figure 1 --------------------------------------------------
create_figure1 <- function(data) {
  data %>%
    filter(Mechanism != 'Total') %>%
    mutate(Years = YearGroup) %>%
    ggplot(aes(x = Age, y = CR, linetype = Sex, color = Years)) +
    geom_smooth(aes(fill = Years), size = 2, method = "loess", alpha = 0.6) +
    facet_wrap(~Mechanism) +
    scale_y_continuous(breaks = seq(0, 40, 5)) +
    scale_color_manual(values = year_colors) +
    scale_fill_manual(values = year_colors) +
    scale_linetype_manual(values = c('dotted', "solid")) +
    labs(
      title = "Suicide Deaths per 100,000 by Age and Sex over Time\n",
      x = "Age",
      y = "Suicide Deaths per 100,000\n",
      linetype = "",
      color = "",
      fill = ""
    ) +
    publication_theme() +
    guides(
      linetype = guide_legend(keywidth = 4, keyheight = 1,
                              override.aes = list(
                                color = 'black', fill = c(NA, NA))
                              ),
      color = guide_legend(title = "", override.aes = list(size = 10), color=F)
    ) 
  }

# Function to create Figure 2 --------------------------------------------------
create_figure2 <- function(data) {
  data %>%
    ggplot(aes(x = age_code, y = crude_rate, color = grade, linetype = gender)) +
    xlim(10, 84) +
    scale_color_manual(values = gun_colors) +
    scale_fill_manual(values = gun_colors) +
    scale_linetype_manual(values = c('dotted', 'solid')) +
    geom_smooth(aes(fill = grade), size = 2, method = "loess", alpha = 0.6) +
    facet_wrap(~mech) +
    scale_y_continuous(breaks = seq(0, 50, 5)) +
    labs(
      title = "Suicide Deaths per 100,000 by Age \nAcross Gun-Permissive and Gun-Strict States",
      subtitle = "Data Years: 2018 to 2022, United States, All Races, All Ethnicities\n",
      x = "Age",
      y = "Suicide Deaths per 100,000\n",
      color = "",
      fill = "",
      linetype = ''
    ) +
    publication_theme() +
    guides(
      fill = guide_legend(keywidth = 1, keyheight = 1),
      linetype = guide_legend(keywidth = 4, keyheight = 1,
                              override.aes = list(color = 'black', fill = c(NA, NA))),
      colour = guide_legend(keywidth = 3, keyheight = 1)
    ) +
    theme(legend.position = 'bottom')
}

# Main execution ---------------------------------------------------------------
main <- function() {
  # Load data
  WISQARS <- readRDS("outputs/clean data/WISQARS/WISQARS.rds")
  WONDER <- readRDS("outputs/clean data/WONDER/WONDER.rds")
  
  # Create and save Figure 1
  fig1 <- create_figure1(WISQARS)
  ggsave("outputs/figures/figure1.png", 
         plot = fig1, width = 15, height = 8, dpi = 300)
  
  # Create and save Figure 2
  fig2 <- create_figure2(WONDER)
  ggsave("outputs/figures/figure2.png", 
         plot = fig2, width = 15, height = 8, dpi = 300)
}

# Run the main function --------------------------------------------------------
main()

################################################################################
