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
    reactions <-set_names(reactions, reaction_names)
  
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
