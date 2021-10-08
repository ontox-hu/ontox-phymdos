library(reticulate)
library(here)

# Open python in R and load sbtab converter
virtualenv_create("ontox-app")
virtualenv_install("ontox-app", "sbtab")
use_virtualenv("ontox-app", required = TRUE)

# Load in file to convert
file_test <- here::here('app', 'physmap.tsv')