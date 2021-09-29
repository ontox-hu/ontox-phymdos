library(reticulate)
library(evaluate)
library(here)
library(rPython)

# Open python in R and load sbtab converter
conda_create("ontox-app", python_version = "3.8")
conda_install("ontox-app", "sbtab", pip = FALSE)
use_condaenv("ontox-app", required = TRUE)

# Load in file to convert
file_test <- here::here('app', 'physmap.tsv')
