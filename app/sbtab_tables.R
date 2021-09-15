## define rhandsontables for SBTab app

library(tidyverse)
## tables: 
sbtab_definitions <- readr::read_tsv("definitions.tsv", 
                                     skip = 1, col_names = TRUE
)

## recode `Type` column
sbtab_definitions <- sbtab_definitions %>%
  mutate(r_format = ifelse(`!Format` == "string", "character", `!Format`)) %>%
  mutate(r_format = ifelse(`!Format` == "float", "double", r_format)) %>%
  mutate(r_format = ifelse(`!Format` == "Boolean", "logical", r_format)) 

## split tables
split_def_tables <- split(sbtab_definitions, as_factor(sbtab_definitions$`!IsPartOf`))

## define tablenames to vector and dataframe
table_names <- names(split_def_tables)
table_names_df <- tibble(names(split_def_tables))
names(table_names_df) <- "name"

## test for function
df_ori = split_def_tables[[1]]

## function creates a table from the definitions object
make_sbtab_table_on_definition <- function(df_ori){
  
  table_columns <- df_ori$`!ComponentName` 
  table_spine <- table_columns %>% t() %>% as_tibble()
  names(table_spine) <- table_spine[1,] %>% as.character() 
  
  table_spine[1,] <- df_ori$r_format %>% as.list()
  
  ## test
  #table_column <- table_spine[,1] %>% names()
  df = table_spine
  table_column = names(table_spine)[1]
  
  change_col_type <- function(table_column, df){
    
    type_char <- df[1, table_column] %>%
      as.character()
    
    if(type_char == "character"){
      df[,table_column] <- df[, table_column] %>% as.character()
    } 
    
    if(type_char == "logical"){
      df[,table_column] <- df[,table_column] %>% as.logical()
    }
    
    if(type_char == "double"){
      df[,table_column] <- df[,table_column] %>% as.double()
    }
    
    df[, table_column]
    
  }  
  
  #change_col_type(df = table_spine, table_column = names(table_spine[,1]))
  
  df_fin <- map_df(
    .x = names(table_spine),
    change_col_type,
    df = df
  ) %>% na.omit()
  
  df_fin[1,] <- NA 
  
  
  return(df_fin)
  
}

map(
  .x = split_def_tables,
  make_sbtab_table_on_definition
) -> sbtab_tables_list


## create examples for each table
reaction <- read_csv(
  file.path("sbtab_table_specifications", "reaction_example.tsv"),
  comment = c("!!")
)

## combine
reaction_def <- dplyr::full_join(sbtab_tables_list$Reaction, reaction) %>%
  dplyr::relocate(
    ID,
    ReactionFormula,
    `Identifiers:kegg.reaction`,
    `Gene:Symbol`,
    ReferencePubMed,
    ReferenceDOI
  )

compound <- read_csv(
  file.path("sbtab_table_specifications", "compound_example.tsv"),
  comment = c("!!")
)

## combine
compound_def <- dplyr::full_join(sbtab_tables_list$Compound, compound) %>%
  dplyr::relocate(
    ID,
    Name,
    `Identifiers:kegg.compound`,
    ReferencePubMed,
    ReferenceDOI
  )
