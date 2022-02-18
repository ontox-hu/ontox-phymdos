## helpers to app
source(
  "R/sbtab_tables.R"
)

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

## read tables from sbtab file and convert to a list
read_sbtab <- function(file, na = ""){
  # read in file and create empty list and name vector
  sbtab <- read_lines(file, lazy = FALSE)
  sbtab <- append(sbtab, "") # every table needs an empty line below it
  tables <- list()
  name <- vector()
  c = 1 # counter for list item
  for(i in sbtab){
    # detect column name line
    if(str_detect(i, "^\\!(?!\\!)")){
      # set table name and create table with column names
      name <- append(name, table_names[which(table_names == str_extract(sbtab[which(i == sbtab)-1], table_names))])
      tables <- append(tables, list(str_split(i, "\t")))
      names(tables) <- name
      tables[[c]] <- as_tibble(tables[[c]], .name_repair = "minimal") %>% t() %>% as_tibble(.name_repair = "minimal")
      names(tables[[c]]) <- tables[[c]][1,] %>% as.character()
      # get table content and write to table
      tab_content <- sbtab[(which(i==sbtab)+1):(which(""==sbtab)[c]-1)]
      for(l in tab_content){
        vector <- tables[[c]][1,] %>% unlist
        vector[1:length(vector)] <- unlist(strsplit(l, "\t"))
        vector <- vector %>% t() %>% as_tibble()
        tables[[c]][which(l == tab_content),] <- vector
      }
      # remove "!" from column names
      names(tables[[c]]) <- str_remove_all(names(tables[[c]]), "!")
      c = c+1
      closeAllConnections()
    } 
  }
  return(tables)
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
      dplyr::select(`!Name`,`!Description`) %>%#,`!Example`)%>%
      dplyr::rename(Name = `!Name`,
                    Description = `!Description`)#,
                    #xample = `!Example`)
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
