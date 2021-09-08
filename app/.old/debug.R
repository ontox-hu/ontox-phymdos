## debugging for running code inside the shiny app
source(
  here::here(
    "app",
    "helpers.R"
  )
)

## chose a different index to slect a different data file as input for the debugging session
data_files <- list.files(
  here::here(
    "data-raw", 
    "D030"
    ), 
  full.names = TRUE,
  pattern = ".xlsx"
  )

data_files
file_chose <- 2


input <- list(
  file1 = data_files[[file_chose]],
  header = TRUE,
  sheet_name = 1,
  skip = 0,
  control = "controlNegative",
  side = "less",
  variance_heterogeneity = TRUE,
  conc_as_categorical = FALSE,
  drc_model = "LL.3"
)

file <- tibble(
  datapath = data_files[[file_chose]]
)

file$datapath

f <- get("LL.4")

