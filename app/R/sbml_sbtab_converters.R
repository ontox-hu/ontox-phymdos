## attnode builds lists that xml2 transforms into <tag attr1=... attr2=.../> 
# this function was provided by Thomas Leuchtefeld
attnode <- function(tag, children = list(), ...){list(structure(children, ...)) %>% set_names(tag)}

## function that checks if column exists. If TRUE, returns column. If FALSE, returns NULL
check <- function(x, col){if(col %in% names(x)){x[[col]]}}

## SBOterm named vectors for species, reactions and modifiers 
species_SBOterms <- c(
  "protein" = "SBO:0000252", 
  "receptor" = "SBO:0000244", 
  "ion channel" = "SBO:0000284", 
  "truncated protein" = "SBO:0000252", 
  "gene" = "SBO:0000243", 
  "rna" = "SBO:0000278", 
  "antisense rna" = "SBO:0000334", 
  "phenotype" = "SBO:0000358", 
  "ion" = "SBO:0000327", 
  "simple molecule" = "SBO:0000247", 
  "drug" = "SBO:0000298", 
  "unknown" = "SBO:0000285", 
  "complex" = "SBO:0000253", 
  "degraded" = "SBO:0000291")

reaction_SBOterms <- c(
  "state transition" = "SBO:0000176",
  "known transition omitted" = "SBO:0000395",
  "unknown transition" = "SBO:0000396",
  "transcription" = "SBO:0000183",
  "translation" = "SBO:0000184",
  "transport" = "SBO:0000185",
  "heterofimer association" = "SBO:0000177",
  "dissociation" = "SBO:0000180",
  "truncation" = "SBO:0000178")

modifier_SBOterms <- c(
  "catalysis" = "SBO:0000013",
  "unknown catalysis" = "SBO:0000462",
  "inhibition" = "SBO:0000537",
  "unknown inhibition" = "SBO:0000536",
  "physical stimulation" = "SBO:0000459",
  "modulation" = "SBO:0000594",
  "trigger" = "SBO:0000461")

## write sbml from list of sbtab-label tables 
# input is list of tables conatining sbtab labels - output is xml_document as per the xml2 package 
write_sbml <- function(sbtablist){
  # remove empty rows from sbtablist tables
  for(t in 1:length(sbtablist)){
    for(r in nrow(sbtablist[[t]]):1){
      if(!FALSE %in% ((sbtablist[[t]][r,1:length(sbtablist[[t]])] == "")|
                       is.na(sbtablist[[t]][r,1:length(sbtablist[[t]])]))
         ){
        sbtablist[[t]] <- sbtablist[[t]][-r,]
      }
    }
  }

  # listOfCompartments - compartments have [metaid, id, size, units]
  # make sure 'size' column is not empty
  sbtablist$Compartment$Size[which(sbtablist$Compartment$Size == "")] <- "1.0"
  listOfCompartments <- sbtablist$Compartment %>% 
    select(metaid = ID, id = ID, size = Size, name = Name, units = Unit) %>% 
    distinct() %>% 
    pmap(~attnode("compartment", ...)) %>%
    purrr::flatten()
  
  # listOfSpecies - species have [metaid id name compartment initialAmount]
  # reformat types and fill empty spots 
  sbtablist$Species$Type <- sbtablist$Species$Type %>% tolower() %>% str_replace_all("_", " ")
  sbtablist$Species$Type[which((sbtablist$Species$Type == "")|(is.null(sbtablist$Species$Type)))] <- "simple molecule"
  # create listofSpecies
  listOfSpecies <- sbtablist$Species %>%
    # switch type names to SBOterms
    mutate(sboTerm = str_replace_all(Type, species_SBOterms)) %>%
    select(metaid = ID, 
           id = ID, 
           name = Name, 
           compartment = Location, 
           initialAmount = if(typeof(check(., "InitialConcentration")) == "NULL"){NULL}else{"InitialConcentration"},
           sboTerm) %>% 
    distinct() %>% 
    pmap(~attnode("species", ...)) %>%
    purrr::flatten()
  
  # individual reactions for listOfReactions
  # seperate the reaction formulas into two columns 
  ## this option can be dropped if the table format in Sysrev/Phymdos is changed from 'ReactionFormula' to 'reactant' and 'product'
  if("ReactionFormula" %in% names(sbtablist$Reaction)){
    reactions <- sbtablist$Reaction %>%
      separate(ReactionFormula, into = c("Reactants", "Products"), sep = "<=>")
  }else{
    reactions <- sbtablist$Reaction
  }
  
  # check if table contains ModifierType column and fill empty cells in ModifierType 
  reactions <- if(is.null(check(reactions, "ModifierType"))){reactions %>% mutate(ModifierType = "")
  }else{reactions %>% mutate(ModifierType = {tolower(ModifierType) %>% str_replace_all("_", " ")})}
  reactions$ModifierType[which((reactions$ModifierType == "")|(is.null(reactions$ModifierType)))] <- "catalysis"
  # create rest of elements
  reactions <- reactions %>% 
    mutate(sboTerm = str_replace_all(ModifierType, modifier_SBOterms)) %>%
    # Select the necesary columns, remove duplicates and arrange by id 
    ## arranging is necesary because of the overlap with listOfReactions
    select(id = ID, 
           listOfReactants = Reactants,
           listOfProducts = Products,
           listOfModifiers = Modifiers,
           sboTerm) %>% 
    distinct() %>%
    arrange(id) %>%
    # Change format of the table so reactions can get a unique metaid, also group by id and split into list
    pivot_longer(cols = c("listOfReactants","listOfProducts", "listOfModifiers"), 
                 names_to = "type", 
                 values_to = "species") %>%
    filter(str_detect(species, "")) %>%
    separate_rows(species, sep = ",") %>%
    cbind(metaid = paste0("CDMT", 1:nrow(.))) %>% 
    mutate(species = trimws(species)) %>% 
    group_by(id) %>% 
    group_split() %>% 
    # Separate reactions into SBML formatted list
    lapply(function(x){
      apply(unique(x[3]), 1, function(y){
        lapply(y, function(z){
          if(z == "listOfModifiers"){
            x[which(x[3] == z),] %>% 
              select(metaid, species, sboTerm) %>%
              pmap(~attnode("modifierSpeciesReference",...)) %>% 
              purrr::flatten()
          }else{
            x[which(x[3] == z),] %>% 
              select(metaid, species) %>%
              pmap(~attnode("speciesReference",...)) %>% 
              purrr::flatten()
          }
        })
      }) %>% 
        purrr::flatten() %>%
        set_names(unique(x[[3]]))
    })
    # pull reaction names
    reaction_names <- pull(arrange(distinct(select(sbtablist$Reaction, ID)), ID))
    # remove empty names
    reaction_names <- reaction_names[nzchar(reaction_names)]
    # give individual reactions corresponding reaction name
    reactions <- set_names(reactions, reaction_names)
  
  # listOfReactions - reactions have [metaid id reversible]
  # reformat types and fill empty spots 
  sbtablist$Reaction$Type <- sbtablist$Reaction$Type %>% tolower() %>% str_replace_all("_", " ")
  sbtablist$Reaction$Type[which((sbtablist$Reaction$Type == "")|(is.null(sbtablist$Reaction$Type)))] <- "state transition"
  # create listofreactions 
  listOfReactions <- sbtablist$Reaction %>%
    mutate(IsReversible = tolower(IsReversible), 
           sboTerm = str_replace_all(Type, reaction_SBOterms)) %>%
    select(metaid = ID, id = ID, reversible = IsReversible, sboTerm) %>%
    distinct() %>%
    arrange(id) %>%
    pmap(~attnode("reaction", ..., children = purrr::flatten(reactions[.]))) %>%
    purrr::flatten()
  
  # write final model and include isolated nodes 
  model <- attnode(tag = "model", metaid = "documentname", id = "documentname",
                   children = lst(listOfCompartments, listOfSpecies, listOfReactions))
  sbml.root <- attnode(tag = "sbml", xmlns = "http://www.sbml.org/sbml/level2/version4", level = 2, version = 4,
                       children = model)
  sbml_model <- as_xml_document(sbml.root)
  return(sbml_model)
}

## create sbtab tables from sbml file
# input is sbml formatted xml - output is a list containing Compartment, Species and Reaction tables
read_sbml <- function(sbml){
  if(is.character(sbml)){
    sbml <- read_xml(sbml)
  }
  # create list from sbml
  tablist <- as_list(sbml)
  
  # create empty tables for Species, Compartment, Reaction and individual reactions
  Compartment <- tibble()
  Species <- tibble()
  Reaction <- tibble()
  reaction_species <- tibble(Reactants = as.character(1:length(tablist[[1]][[1]][["listOfReactions"]])), Products = "", Modifiers = "", ModifierType = "")
  
  # convert compartments to table
  for(i in 1:length(tablist[[1]][[1]][["listOfCompartments"]])){
    Compartment <- bind_rows(Compartment, attributes(tablist[[1]][[1]][["listOfCompartments"]][[i]]))
  }
  
  # correct column names
  Compartment <- Compartment %>% 
    transmute(
      "ID" = check(., "id"), 
      "Name" = check(., "name"), 
      "Size" = check(., "size"), 
      "Unit" = check(., "units"), 
      "OuterCompartment" = check(., "outside"))
  
  # convert Species to table
  for(i in 1:length(tablist[[1]][[1]][["listOfSpecies"]])){
    Species <- bind_rows(Species, as_tibble(attributes(tablist[[1]][[1]][["listOfSpecies"]][[i]])))
  }
  
  # correct column names
  Species <- Species %>% transmute(
    "ID" = check(., "id"), 
    "Name" = check(., "name"), 
    "Type" = if(is.null(check(., "sboTerm"))){
      NULL
    }else{
      str_replace_all(sboTerm, setNames(names(species_SBOterms), species_SBOterms))
    },
    "InitialValue" = check(., "initialAmount"), 
    "Unit" = check(., "units"), 
    "Location" = check(., "compartment"), 
    "IsConstant" = check(., "constant"), 
    "HasOnlySubstanceUnits" = check(., "hasOnlySubstanceUnits"))
  
  # convert Reactions to table
  for(i in 1:length(tablist[[1]][[1]][["listOfReactions"]])){
    # get reactions
    Reaction <- bind_rows(Reaction, as_tibble(attributes(tablist[[1]][[1]][["listOfReactions"]][[i]])))
    # create a counter for column selection after skipping annotations
    count <- 1
    for(c in 1:length(tablist[[1]][[1]][["listOfReactions"]][[i]])){
      # get reactants and ModifierType SBOterms
      ## skip annotations
      if(!names(tablist[[1]][[1]][["listOfReactions"]][[i]][c]) == "annotation"){
        reaction_species[i,count] <- 
          paste(lapply(tablist[[1]][[1]][["listOfReactions"]][[i]][[c]], function(x){
            attributes(x)[["species"]]}), collapse = ", ")
          #attributes(tablist[[1]][[1]][["listOfReactions"]][[i]][[c]][[1]])[["species"]]
        if(is.null(check(attributes(tablist[[1]][[1]][["listOfReactions"]][[i]][[c]][[1]]), "sboTerm"))==FALSE){
          reaction_species[i,"ModifierType"] <-
            attributes(tablist[[1]][[1]][["listOfReactions"]][[i]][[c]][[1]])[["sboTerm"]]
        }
        count <- count+1
      }
    }
  }
  # bind reactants to reactions
  Reaction <- Reaction[-1] %>% distinct() %>% cbind(reaction_species)
  
  # correct Reaction names
  Reaction <- Reaction %>% 
    transmute(
      "ID" = check(., "id"), 
      "Name" = check(., "name"), 
      "Type" = if(is.null(check(., "sboTerm"))){
        NULL
      }else{
        str_replace_all(sboTerm, setNames(names(reaction_SBOterms), reaction_SBOterms))
      },
      "IsReversible" = check(., "reversible"),
      Reactants,
      Products,
      Modifiers,
      "ModifierType" = if(is.null(check(., "ModifierType"))){
        NULL
      }else{
        str_replace_all(ModifierType, setNames(names(modifier_SBOterms), modifier_SBOterms))
      },
      "Location" = check(., "location"), 
      "KineticLaw" = check(., "kineticLaw"))
  
  sbtab <- list("Compartment"= Compartment, "Species" = Species, "Reaction" = Reaction)
  return(sbtab)
}

#' Write SBtab structured tables into SBtab .tsv file
#'
#' \code{write_sbtab} writes SBTab structured table lists into a .tsv SBtab file. 
#'
#' @param sbtablist The list of tables to be written into the .tsv SBtab file
#' @param docname The name of the document
#'
#' @return A .tsv structured SBtab file
#' @export
#'
#' @examples
#' sbtablist <- list_of_sbtab_tables
#' docname <- sbtab_document_name
#' 
#' write_sbtab(sbtablist, docname) 
write_sbtab <- function(sbtablist, docname){
  # set documentname header for SBtab output file
  paste0('!!!SBtab Document="', 
         if(is_empty(docname)){"Phymdos_SBtabfile"}else{docname}, 
         '"') |>
    as.character() |>
    write_lines(file = "physmap.tsv")
  
  # write tables into SBtab output file
  for(table in names(sbtablist)){
    # write table header for file
    paste0('!!SBtab TableID="t_', table, '"', 
           ' SBtabVersion="1.0"', 
           ' Document="', if(is_empty(docname)){"Phymdos_SBtabfile"}else{docname}, '"',
           ' TableType="', table, '"',
           ' TableName="', table, '"') |>
      write_lines(file = "physmap.tsv", append = TRUE)
    # write table contents to file
    write_tsv(set_cols(sbtablist[[table]]), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
    # append whiteline after table
    write_lines("", file = "physmap.tsv", append = TRUE)
  }
}

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
  # in older SBTab files 'Species' columns are called 'Compound'
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
