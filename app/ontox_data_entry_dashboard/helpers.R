## helpers to app

library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)

source(
  "sbtab_tables.R"
  )


#' @title Export app sbtab table to SbTab S4 object
#' @param df A dataframe
#' @param SbTabMeta Object of formal class SbTabMeta
#' @param SbtabColNames Object of formal class SbTabColNames
#' @return SbTab class S4 object
#' @author Marc A.T. Teunis  
#' @export

sbtab_app_table_to_SbTab <- function(){
  
  
}




values <- list() 

setHot <- function(x) 
  values[["Reaction"]] <<- x 

DF <- reaction_def
