## helpers to app

library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(shinyjs)
library(shinyBS)

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

## add ! to colnames
set_cols <- function(x){
  names <- paste0("!", colnames(x))
  x <- rlang::set_names(x, names)
}

## read tables from sbtab file and convert to a list
read_sbtab <- function(file){
  # read in file and create empty list and name vector
  sbtab <- read_lines(file)
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
      names(tables[[c]]) <- names(sbtab_tables_list[[names(tables[c])]])
      c = c+1
    } 
  }
  return(tables)
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
