### tests

library(readxl)
library(janitor)
library(tidyverse)

readxl::read_xlsx(
  here::here(
    "data-raw",
    "D030",
    "CE.LIQ.118_tidydata.xlsx"
  )
) %>% 
  janitor::clean_names() %>%
  write_csv(
    file = here::here(
      "app",
      "tests",
      "data_test.csv"
    )
  )
  
