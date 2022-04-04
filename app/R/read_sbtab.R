## read tables from sbtab file and convert to a list
read_sbtab <- function(file, na = ""){
  # read in file and create empty list and name vector
  sbtab <- read_lines(file, lazy = FALSE)
  # check if the uploaded file is an SBtab file
  if(startsWith(sbtab[1], "!!!SBtab Document=")){
    # check if the sbtab file contains the correct tables
    if(str_detect(paste(sbtab, collapse = " "),  '!!SBtab TableID="t_Compound"') &&
       str_detect(paste(sbtab, collapse = " "),  '!!SBtab TableID="t_Reaction"') &&
       str_detect(paste(sbtab, collapse = " "),  '!!SBtab TableID="t_Compartment"')){
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
          tables[[c]] <- as_tibble(tables[[c]], .name_repair = "minimal") %>% t() %>% as_tibble(.name_repair = "minimal")
          names(tables[[c]]) <- tables[[c]][1,] %>% as.character()
          # get table content and write to table
          tab_content <- sbtab[(which(i==sbtab)+1):(which(""==sbtab)[c]-1)]
          for(l in tab_content){
            vector <- tables[[c]][1,] %>% unlist
            suppressWarnings(vector[1:length(vector)] <- unlist(strsplit(l, "\t")))
            vector <- vector %>% t() %>% as_tibble()
            tables[[c]][which(l == tab_content),] <- vector
          }
          # remove "!" from column names
          names(tables[[c]]) <- str_remove_all(names(tables[[c]]), "!")
          c = c+1
          closeAllConnections()
        } 
      }
      # check for wrong columns in tables
      if(FALSE %in% str_detect(paste(names(sbtab_tables_list[["Reaction"]]), collapse = " "), names(tables[["Reaction"]])) ||
         FALSE %in% str_detect(paste(names(sbtab_tables_list[["Compound"]]), collapse = " "), names(tables[["Compound"]])) ||
         FALSE %in% str_detect(paste(names(sbtab_tables_list[["Compartment"]]), collapse = " "), names(tables[["Compartment"]]))
      ){
        # list wrong columns in reaction table
        if(FALSE %in% str_detect(paste(names(sbtab_tables_list[["Reaction"]]), collapse = " "), names(tables[["Reaction"]]))
        ){
          reactionError <- paste0("The Reaction table contains faulty collumns:", 
                                  " '",
                                  paste(suppressWarnings(
                                    names(tables[["Reaction"]])[
                                      which(names(tables[["Reaction"]]) != names(sbtab_tables_list[["Reaction"]]))
                                    ]), collapse = "', '"), 
                                  "'. Please remove the columns to proceed."
          )
        }else{reactionError <- NULL}
        # list wrong columns in compound table
        if(FALSE %in% str_detect(paste(names(sbtab_tables_list[["Compound"]]), collapse = " "), names(tables[["Compound"]]))
        ){
          compoundError <- paste0("The Compound table contains faulty collumns:", 
                                  " '",
                                  paste(suppressWarnings(
                                    names(tables[["Compound"]])[
                                      which(names(tables[["Compound"]]) != names(sbtab_tables_list[["Compound"]]))
                                    ]), collapse = "', '"), 
                                  "'. Please remove the columns to proceed."
          )
        }else{compoundError <- NULL}
        # list wrong columns in compartment table
        if(FALSE %in% str_detect(paste(names(sbtab_tables_list[["Compartment"]]), collapse = " "), names(tables[["Compartment"]]))
        ){
          compartmentError <- paste0("The Compartment table contains faulty collumns:", 
                                     " '",
                                     paste(suppressWarnings(
                                       names(tables[["Compartment"]])[
                                         which(names(tables[["Compartment"]]) != names(sbtab_tables_list[["Compartment"]]))
                                       ]), collapse = "', '"), 
                                     "'. Please remove the columns to proceed."
          )
        }else{compartmentError <- NULL}
        # stop on wrong columns
        stop(paste("\n", reactionError, "\n", compoundError, "\n", compartmentError))
      }
    }else{
      # stop on incorrect tables
      stop("This SBtab file does not contain the correct tables. Please make sure the file contains a Reaction, a Compound, and a Compartment table.")
    }
  }else{
    # stop on incorrect file 
    stop("This is not an SBtab file. Please make sure the file uses the SBtab format.")
  }
  return(tables)
}