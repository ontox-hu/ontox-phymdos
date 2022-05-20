# Open python in R and load sbtab converter
virtualenv_create("phymdos2")
virtualenv_install(envname = "phymdos2", c("sbtab", "requests"))
use_virtualenv("phymdos2", required = TRUE)

#load in minerva status function
source_python("py/minerva_status.py")
