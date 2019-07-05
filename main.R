# DADI initialization

## Starting 

# Clean environment
rm(list=ls())
gc()

# Disable scientific notation
options(scipen=999)

# Change prompt
options(prompt="DADI> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("scripts/utiles.R")

# Set report period
yearmonth <- 201903

# Set model name
model_alias <- paste0("dadi_", yearmonth)

# Set up paths
set_environment() 
