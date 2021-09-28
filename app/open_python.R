# Open python in R and load sbtab converter
library(reticulate)
conda_create("ontox-app", python_version = "3.8")
conda_install("ontox-app", "sbtab", pip = TRUE)
use_condaenv("ontox-app", required = TRUE)