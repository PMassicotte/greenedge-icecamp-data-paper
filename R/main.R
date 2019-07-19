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
library(magick) # For images
library(rnaturalearth)
library(ggspatial)
library(ggpmthemes)
library(ggisoband)
library(readxl)
library(patchwork)

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_poppins())

# Figures -----------------------------------------------------------------

source("R/fig1.R")
source("R/fig2.R")
source("R/fig3.R")
source("R/fig4.R")
source("R/fig5.R")
source("R/fig6.R")
source("R/fig7.R")
source("R/fig8.R")
source("R/fig9.R")
source("R/fig10.R")
source("R/fig11.R")
source("R/fig12.R")
source("R/fig13.R")
