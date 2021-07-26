# Load packages
library(tidyverse)
library(ggplot2)

# Read Data
file_1 <- file.path("data", "lake_metadata.csv")
lake_metadata <- read_csv(file_1)

file_2 <- file.path("data", "lake_surface_temp_preds.csv")
surface_temp_pred <- read_csv(file_2)



# Explore Data
glimpse(lake_metadata)
glimpse(surface_temp_pred)
