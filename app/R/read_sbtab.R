#' Read SBTab file into table list
#'
#' \code{read_sbtab} reads SBTab files into a list of the tables that are in the file. 
#' The function uses a definitions.tsv file to define these tables. Column, and table names can therefore not 
#' differ from those listed in definitions.tsv.
#'
#' @param sbtab_file An SBTab file to convert to table list
#' @param definitions The definitions file used to define the SBtab tables.
#'
#' @return A list containing the tables listed in the SBTab file
#' @export
#'
#' @examples
#' sbtab_file <- "path/to/sbtab_file"
#' definitions <- "path/to/definitions.tsv"
#' 
#' sbtab_tables <- read_sbtab(sbtab_file, definitions)
read_sbtab <- function(sbtab_file, definitions){
  ## read sbtab_file
  sbtab <- read_lines(sbtab_file, lazy = FALSE)
  # change instances of the word 'Compound' to the word 'Species'
  ## in older SBTab files 'Species' columns are called 'Compound'
  sbtab <- str_replace_all(sbtab, "Compound", "Species")
  
  ## read definitions and define tables: 
  sbtab_definitions <- readr::read_tsv(definitions, skip = 1, col_names = TRUE, show_col_types = FALSE)
  # recode `Type` column
  sbtab_definitions <- sbtab_definitions |>
    mutate(r_format = ifelse(`!Format` == "String", "character", `!Format`)) |>
    mutate(r_format = ifelse(`!Format` == "Float", "double", r_format)) |>
    mutate(r_format = ifelse(`!Format` == "Boolean", "logical", r_format)) 
  # split tables
  split_def_tables <- split(sbtab_definitions, as_factor(sbtab_definitions$`!Parent`))
  split_def_tables <- split_def_tables[names(split_def_tables) != "SBtab"]
  # define table names
  table_names <- names(split_def_tables)
  
  ## transform sbtab_file contents to tables
  # check if the uploaded file is an SBtab file
  if(startsWith(sbtab[1], "!!!SBtab Document")){
    # check if the sbtab file contains the correct tables
    if(str_detect(paste(sbtab, collapse = " "),  'TableType="Species"|TableType=\'Species\'|TableType="Compound"|TableType=\'Compound\'') &&
       str_detect(paste(sbtab, collapse = " "),  'TableType="Reaction"|TableType=\'Reaction\'') &&
       str_detect(paste(sbtab, collapse = " "),  'TableType="Compartment"|TableType=\'Compartment\'')){
      # make sure every table has an empty line below it
      sbtab <- append(sbtab, "") 
      # create empty table list and table name vector
      tables <- list()
      name <- vector()
      # create counter for list item
      c = 1 
      for(i in sbtab){
        # detect column name line
        if(str_detect(i, "^\\!(?!\\!)")){
          # set table name and create table with column names
          name <- append(name, table_names[which(table_names == str_extract(sbtab[which(i == sbtab)-1], table_names))])
          tables <- append(tables, list(str_split(i, "\t")))
          names(tables) <- name
          tables[[c]] <- as_tibble(tables[[c]], .name_repair = "minimal") |> t() |> as_tibble(.name_repair = "minimal")
          names(tables[[c]]) <- tables[[c]][1,] |> as.character()
          # get table content and write to table
          tab_content <- sbtab[(which(i==sbtab)+1):(which(("" == sbtab)|(" " == sbtab))[c]-1)]
          # paste a whitespace at the end of each tab_content element to get correct length for vector
          tab_content <- sapply(tab_content, function(x){paste0(x, " ")}, USE.NAMES = FALSE)
          for(l in tab_content){
            vector <- tables[[c]][1,] |> unlist()
            suppressWarnings(vector[1:length(vector)] <- unlist(strsplit(l, "\t")) |> trimws())
            vector <- vector |> t() |> as_tibble()
            tables[[c]][which(l == tab_content),] <- vector
          }
          # remove empty rows from table
          for(x in 1:nrow(tables[[c]])){
            if(!FALSE %in% (tables[[c]][x,1:length(tables[[c]])] == "")){
              tables[[c]] <- tables[[c]][-x,]
            }
          }
          # remove "!" from column names and make sure they start capitalised
          names(tables[[c]]) <- str_remove_all(names(tables[[c]]), "!") 
          names(tables[[c]]) <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", names(tables[[c]]), perl = TRUE)
          c = c+1
          closeAllConnections()
        } 
      }
      # if Reaction table contains ReactionFormula column, split into Reactants and Products
      ## older or external SBTab files will contain this column
      if("ReactionFormula" %in% names(tables$Reaction)){
        tables$Reaction <- tables$Reaction %>%
          separate(ReactionFormula, into = c("Reactants", "Products"), sep = "<=>") %>%
          mutate(Reactants = trimws(Reactants), 
                 Products = trimws(Products))
      }
      # check for wrong columns in tables
      if(FALSE %in% str_detect(paste(split_def_tables$Reaction$`!Name`, collapse = " "), names(tables[["Reaction"]])) ||
         FALSE %in% str_detect(paste(split_def_tables$Species$`!Name`, collapse = " "), names(tables[["Species"]])) ||
         FALSE %in% str_detect(paste(split_def_tables$Compartment$`!Name`, collapse = " "), names(tables[["Compartment"]]))
      ){
        # list wrong columns in reaction table
        if(FALSE %in% str_detect(paste(split_def_tables$Reaction$`!Name`, collapse = " "), names(tables[["Reaction"]]))
        ){
          reactionError <- paste0("The Reaction table contains faulty collumns:", 
                                  " '",
                                  paste(suppressWarnings(
                                    names(tables[["Reaction"]])[
                                      which(names(tables[["Reaction"]]) != split_def_tables$Reaction$`!Name`)
                                    ]), collapse = "', '"), 
                                  "'. Please remove the columns to proceed."
          )
        }else{reactionError <- NULL}
        # list wrong columns in Species table
        if(FALSE %in% str_detect(paste(split_def_tables$Species$`!Name`, collapse = " "), names(tables[["Species"]]))
        ){
          SpeciesError <- paste0("The Species table contains faulty collumns:", 
                                  " '",
                                  paste(suppressWarnings(
                                    names(tables[["Species"]])[
                                      which(names(tables[["Species"]]) != split_def_tables$Species$`!Name`)
                                    ]), collapse = "', '"), 
                                  "'. Please remove the columns to proceed."
          )
        }else{SpeciesError <- NULL}
        # list wrong columns in compartment table
        if(FALSE %in% str_detect(paste(split_def_tables$Compartment$`!Name`, collapse = " "), names(tables[["Compartment"]]))
        ){
          compartmentError <- paste0("The Compartment table contains faulty collumns:", 
                                     " '",
                                     paste(suppressWarnings(
                                       names(tables[["Compartment"]])[
                                         which(names(tables[["Compartment"]]) != split_def_tables$Compartment$`!Name`)
                                       ]), collapse = "', '"), 
                                     "'. Please remove the columns to proceed."
          )
        }else{compartmentError <- NULL}
        # stop on wrong columns
        stop(paste("\n", reactionError, "\n", SpeciesError, "\n", compartmentError))
      }
    }else{
      # stop on incorrect tables
      stop("This SBtab file does not contain the correct tables. Please make sure the file contains a Reaction, a Species, and a Compartment table.")
    }
  }else{
    # stop on incorrect file 
    stop("This is not an SBtab file. Please make sure the file uses the SBtab format.")
  }
  return(tables)
}
