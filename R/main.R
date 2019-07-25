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
library(googlesheets)
library(ggforce)
library(glue)

rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font familly

loadfonts(quiet = TRUE)
theme_set(theme_poppins())

# Figures -----------------------------------------------------------------

source("R/fig01.R")
source("R/fig02.R")
source("R/fig03.R")
source("R/fig04.R")
source("R/fig05.R")
source("R/fig06.R")
source("R/fig07.R")
source("R/fig08.R")
source("R/fig09.R")
source("R/fig10.R")
source("R/fig11.R")
source("R/fig12.R")
source("R/fig13.R")
source("R/fig14.R")
