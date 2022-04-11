## helpers to app
source(
  "R/sbtab_tables.R"
)

source(
  "R/read_sbtab.R"
)

## display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

## create dynamic sub menu
update_submenu <- function(local) {
  lapply(split(local$subitems, seq(nrow(local$subitems))), function(x) {
    menuSubItem(x$name, tabName = paste0("tab_", x$id))
  })
}

## create static tab list
tab_list_ui <- function() {
  # concatenate static tabs
  items <- c(
    list(
      tabItem(
        tabName = "setup",
        uiOutput("mysetup")
      )
    ),
    list(
      tabItem(
        tabName = "select_tables",
        uiOutput("mytables")
      )
    ),
    lapply(table_names, function(id) {
      tabItem(
        tabName = paste0("tab_", id), 
        uiOutput(paste0("sub_", id))
      )
    }),
    list(
      tabItem(
        tabName = "help",
        uiOutput("myhelp")
      )
    ),
    list(
      tabItem(
        tabName = "info",
        uiOutput("myinfo")
      )
    )
  )
  # render
  do.call(tabItems, items)
}

## add ! to colnames
set_cols <- function(x){
  names <- paste0("!", colnames(x))
  x <- rlang::set_names(x, names)
}

## Check if a table is filled and open filled tables in the sidebar
open_tabs <- function(table){
  if(length(is.na(table)) == length(table)){
    TRUE
    }else if(length(which(table == "")) == length(table)){
      TRUE
      }else{    
        FALSE
      }
  }

## Output the table description with the tables
outputTableDescription <- function(tableTitle){
  DT::renderDataTable({
    split_def_tables[[tableTitle]]%>%
      dplyr::select(`!Name`,`!Description`,`!Example`) %>%
      dplyr::rename(Name = `!Name`,
                    Description = `!Description`,
                    Example = `!Example`)
    })
  }

## Create individual table pages for adding table by hand
add_tableUI <- function(subitem){
  list(
    bsCollapsePanel("Select columns to include",
                    checkboxGroupInput(paste0(subitem, "_cols"),
                                       "Choose from:",
                                       choices = names(sbtab_tables_list[[subitem]]),
                                       selected = c("ReferenceDOI", 
                                                    "ID", 
                                                    "ReactionID"
                                                    ),
                                       inline = TRUE)
                    ),
    tabItem(
      tabName = subitem,
      fluidRow(
        column( 10,
                rHandsontableOutput(paste0(subitem, "_hot"), 
                                    height = 400, 
                                    width = "100%"),
                offset = 0
        ),
      )
    ),
    actionButton(paste0("goto_download_", subitem), 
                 "Click here to go to the download screen" ),
    br(), br(),
    bsCollapsePanel("Description of table elements",
                    DT::dataTableOutput(paste0("Description", subitem), 
                                        width = "100%")
                    )
    )
  }

## Create individual table pages for adding table by upload
upload_tableUI <- function(subitem, sbtabfile = list()){
  list(
    bsCollapsePanel("Select columns to include",
                    checkboxGroupInput(paste0(subitem, "_cols"),
                                       "Choose from:",
                                       choices = names(sbtab_tables_list[[subitem]]),
                                       selected = c("ReferenceDOI", 
                                                    "ID", 
                                                    "ReactionID", 
                                                    names(sbtabfile[[subitem]][which(sbtabfile[[subitem]][1,] != "")])
                                       ),
                                       inline = TRUE)
    ),
    tabItem(
      tabName = subitem,
      fluidRow(
        column( 10,
                rHandsontableOutput(paste0(subitem, "_hot"), 
                                    height = 400, 
                                    width = "100%"),
                offset = 0
        ),
      )
    ),
    actionButton(paste0("goto_download_", subitem),
                 "Click here to go to the download screen" ),
    br(), br(),
    bsCollapsePanel("Description of table elements",
                    DT::dataTableOutput(paste0("Description", subitem), 
                                        width = "100%")
    )
  )
}

## create vectors for minerva counters
minerva_status = paste("Status: empty")
minerva_progress = paste("Progress: empty")