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




# values <- list() 
# 
# setHot <- function(x, tabTitle) 
#   values[[tabTitle]] <<- x 
# 
# DF <- reaction_def
DF <- list()

## Functions to help display

# displayMenuItem <- function(menuTitle){
#   menuItem(
#     menuTitle,
#     tabName = menuTitle,
#     icon = icon("dashboard")
#   )
# }

displayMenuItemUI <- function(id){
  NS(id)
  tagList(
    menuItem(
      id,
      tabName = id,
      icon = icon("dashboard")
    )
  )
  
}

displayTabContent <- function(tableTitle){
  tabItem(
    tabName = tableTitle,
    fluidRow(
      span(textOutput(paste0(tableTitle,"Meta")), style = "color:red"),
      br(),
      rHandsontableOutput(tableTitle, height = 200, width = 1000)
    ),
    fluidRow(DT::dataTableOutput(paste0(
      "Description", tableTitle
    )), width = 1000)
  )
}

# displayTabContentUI <- function(id){
#   NS(id)
#   tagList(
#     tabItem(
#       tabName = id,
#       fluidRow(
#         span(textOutput(paste0(id,"Meta")), style = "color:red"),
#         br(),
#         rHandsontableOutput(id, height = 200, width = 1000)
#       ),
#       fluidRow(DT::dataTableOutput(paste0(
#         "Description", id
#       )), width = 1000)
#     )
#   )
# }

outputTable <- function(tableTitle){
  renderRHandsontable({rhandsontable(sbtab_tables_list[[tableTitle]])
    
  })
}

outputTableDescription <- function(tableTitle){
  DT::renderDataTable({
    split_def_tables[[tableTitle]]%>%
      dplyr::select(`!ComponentName`,`!Description`,`!Format`)%>%
      dplyr::rename(ComponentName = `!ComponentName`,
                    Description = `!Description`,
                    Format = `!Format`)
  })
}


# ## save changes to table made
# observe({
#   input$saveBtn # update dataframe file each time the button is pressed
#   if (!is.null(values[["Reaction"]])) { # if there's a table input
#     DF <<- values$Reaction
#   }
# })
# 
# observe({
#   if (!is.null(input[[tableTitle]])){
#     DF[[tableTitle]] <- (hot_to_r(input[[tableTitle]]))
#     setHot(DF[[tableTitle]])
#     
#     #  readr::write_csv(DF, "test.csv")
#     
#   } 
# })

# observe({
#   if (!is.null(input$Reaction)) DF$Reaction(hot_to_r(input$Reaction))
# })

# output$Reaction <- renderRHandsontable({
#   rhandsontable(DF$Reaction()) %>% 
#     hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) %>%
#     hot_col("Status", readOnly = FALSE)
# })