library(reticulate)
library(here)

# Open python in R and load sbtab converter
virtualenv_create("ontox-app")
virtualenv_install(envname = "ontox-app", "sbtab")
use_virtualenv("ontox-app", required = TRUE)

# Load in converter functions
source_python("python/sbtab_converters.py")
