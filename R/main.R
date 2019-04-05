# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(extrafont)
library(tidyverse)
library(MBA) # For grid interpolation

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 10, base_family = "IBM Plex Sans"))

# Figures -----------------------------------------------------------------

source("R/fig1.R")
source("R/fig2.R")
source("R/fig3.R")
source("R/fig4.R")
source("R/fig5.R")
source("R/fig6.R")
source("R/fig7.R")
