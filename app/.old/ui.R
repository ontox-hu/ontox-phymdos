##################################################
## ui.R
## UI (User Interface) part of Dashboard C.elegans Shiny app
## Marc A.T. Teunis, PhD
## January 2021
################################################


####################### Packages ###########################################

library(shiny)
library(shinydashboard)
library(tidyverse)
#library(drc)

##################### HELPERS ###############################################

source("helpers.R")

################# HEADER ####################################################

## dashboard header
header <- dashboardHeader(title = "MapR", titleWidth = 400)


############## SIDEBAR #####################################################
## dashboard sidebar
## Table categories

categories <- sbtab_definitions_class@data$`!IsPartOf` %>%
  unique()


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      categories[1],
      tabName = categories[1]),
    menuItem(
      categories[2], 
      tabName = categories[2]),
    menuItem(
      categories[3], 
      tabName = categories[3]),
    menuItem(
      categories[4], 
      tabName = categories[4]),
    menuItem(
      categories[5], 
      tabName = categories[5]),
    menuItem(
      categories[6], 
      tabName = categories[6]),
    menuItem(
      categories[7], 
      tabName = categories[7]),
    menuItem(
      categories[8], 
      tabName = categories[8]),
    menuItem(
      categories[9], 
      tabName = categories[9]),
    menuItem(
      categories[10], 
      tabName = categories[10]),
    menuItem(
      categories[11], 
      tabName = categories[11]),
    menuItem(
      categories[12], 
      tabName = categories[12])
    
  ),
  
  
  ####################### sidebar UI input ###########################################
  
  # Input: Select a file ----
  fileInput("file1", "Choose an Excel File", accept = ".xlsx"),
  # display header of table or not
  checkboxInput("header", "Header", TRUE),
  # select colum-seperator
  textInput("sheet_name", "Sheet name or index", "expParameters"),
  # skip lines?
  numericInput("skip", "Skip lines?", 0),
  # select type of control to be used
  selectInput(
    inputId = "control",
    label = "Control",
    choices = c("controlVehicleA",
                "controlNegative"),
    selected = "controlNegative"
  ),
  
  selectInput(
    inputId = "drc_model",
    label = "DRC Model",
    choices = c("LL.3",
                "LL.4"),
    selected = "LL.3"
  ),

  selectInput(
    inputId = "side",
    label = "Direction of testing",
    choices = c("two.sided",
                "less",
                "greater"),
    selected = "less"
  ),
  
  checkboxInput(
    inputId = "variance_heterogeneity",
    label = "Variance heterogeneity",
    value = TRUE
  ),
  
  checkboxInput(
    inputId = "conc_as_categorical",
    label = "Concentration as categorical",
    value = FALSE
  )
)

######################## BODY #################################################

## dashboard body content
mainPanel(
  rHandsontableOutput("hot")
)


######################## UI ################################################

ui <- dashboardPage(header,
                    sidebar,
                    body)

