##############################################
# Code author: Erin Stearns
# Code objective: Getting familiar with geo data and making maps in R
# Date: 12.14.2017
#############################################

rm(list = ls()) 

# Load necessary packages
x <- c("data.table","reshape2","ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
lapply(x, require, character.only = TRUE)
