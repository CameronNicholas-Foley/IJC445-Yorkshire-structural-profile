################################################################################
#Libraries required (load all before continuing)
################################################################################


#Data processing and ggplot
library(tidyverse)
library(readxl)
library(janitor)
library(ggrepel)

#Visualisation themes
library(ggthemes)

#For Figure 4 Scaling and PCA biplot
library(scales)
library(factoextra)

################################################################################
#Functions
################################################################################


#Assigning region function
region_func <- function(itl_code) {
  case_when(grepl("^..C", itl_code) ~ "North East",
            grepl("^..D", itl_code) ~ "North West",
            grepl("^..E", itl_code) ~ "Yorkshire and The Humber",
            grepl("^..F", itl_code) ~ "East Midlands",
            grepl("^..G", itl_code) ~ "West Midlands",
            grepl("^..H", itl_code) ~ "East",
            grepl("^..I", itl_code) ~ "London",
            grepl("^..J", itl_code) ~ "South East",
            grepl("^..K", itl_code) ~ "South West",
            TRUE ~ itl_code)} 