source("helpers.R")
# library(reticulate)
# use_condaenv("ontox-app", required = TRUE)

## TODO:
# Saving progress made on table
# Reset table when changing map without having to go back and forth? => store multiple input?
# Possibility to re-import table when filling map in multiple times: we will need a database infrastructure? Where are files saved
# Make saving in .xml dependent on having saved in .tsv before
# Proper python (conda?) env to make the xml conversion possible
# Use Shiny modules

## Main UI definition
ui = dashboardPage(
  dashboardHeader(title = "ONTOX - Physiological Maps Data Entry Portal",
                  titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Map details",
        tabName = "MapMetaData"
      ),
      # Show all tables on a different tab
      lapply(table_names,displayMenuItemUI)
    )
  ),
  

  dashboardBody(
    tabItems(
      tabItem(
        # Set all parameters
        tabName = "MapMetaData",
        fluidRow(
          selectInput(
            "MapMetaData",
            "For which map would you like to enter data?",
            c(
              "Test-map (default)" = "To test the app and discover how it works (default)",
              "Liver-bile" = "Liver physiological map: bile synthesis",
              "Liver-steatosis" = "Liver physiological map: steatosis",
              "Liver-glycogenolysis" = "Liver physiological map: glycogenolysis"
            ), selected = "Test-map (default)"
          ),
          span(textOutput("MapMetaData"), style = "color:red")
        ),
        br(),
        selectInput("sbtab_version", "Which SBtab Version do you need (1.0 default)?", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        verbatimTextOutput("sbtab_version"),
        br(),
        actionButton("savefile", "Save all changes made to this map in SBtab (.tsv) format"),
        br(),
        br(),
        actionButton("savexml", "Save all changes made to this map in SBML (.xml) format")
        ),
      
      # Display each individual tab content, corresponding to each SBtab - can we vectorize this?
      displayTabContent(table_names[1]),
      displayTabContent(table_names[2]),
      displayTabContent(table_names[3]),
      displayTabContent(table_names[4]),
      displayTabContent(table_names[5]),
      displayTabContent(table_names[6]),
      displayTabContent(table_names[7]),
      displayTabContent(table_names[8]),
      displayTabContent(table_names[9]),
      displayTabContent(table_names[10]),
      displayTabContent(table_names[11]),
      displayTabContent(table_names[12])
      
    )
  )
)



server = function(input, output) {
  # This is to be able to re-use the input from MapMetaData on other tabs
  output$MapMetaData  <- renderText({ #<- output$ReactionMeta <- output$CompoundMeta
    input$MapMetaData
  })
  for(i in table_names){
    output[[paste0(i,"Meta")]] <- renderText({
      input$MapMetaData
    })
  }


  observeEvent(input$savefile,
               # Save all tables into .tsv SBtab model format
               {
                 # Open file to start writing
                 #TODO: allow filename input by user
                 sink('test.tsv')
                 cat(paste0('!!!SBtab Document="', input$MapMetaData, '"'))
                 cat("\n")
                 # Counter for table ID
                 i=1
                 for(item in table_names){
                   if (!is.null(isolate(input[[item]]))) # this is only true if the user hasn't clicked on the tab. check nrow instead
                   {
                     #Convert to R object
                     x <- hot_to_r(isolate(input[[item]]))
                     # Convert column names to SBtab format
                     colnames(x) <- paste0("!", colnames(x))
                     # Write in table header
                     cat(paste0('!!SBtab TableID="t_',i, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$MapMetaData, '"',' TableType="',item, '"',' TableName="',item, '"'))
                     cat("\n")
                     # Write in table
                     write.table(x, row.names=FALSE, na = "", quote=F, sep="\t") 
                     cat("\n")
                     i=i+1
                     
                   }
                 }
                 # Close connection to file
                 sink()

               })   
  
  # observeEvent(input$savexml,
  #              # Save all tables into SBML .xml format
  #              {
  #               source_python("table_to_sbml.py")
  #               tab_to_sbml("test2.tsv", "test.xml", "test")
  #              })  
  
  # Content of each table - can we vectorize this?
  # observe({
  #   if (!is.null(input[[table_names[[1]]]])) DF[[table_names[[1]]]](hot_to_r(input[[table_names[[1]]]]))
  # })
  output[[table_names[[1]]]] <- outputTable(table_names[[1]])
  output[[paste0("Description",table_names[[1]])]] <- outputTableDescription(table_names[[1]])
  
  output[[table_names[[2]]]] <- outputTable(table_names[[2]])
  output[[paste0("Description",table_names[[2]])]] <- outputTableDescription(table_names[[2]])
  
  output[[table_names[[3]]]] <- outputTable(table_names[[3]])
  output[[paste0("Description",table_names[[3]])]] <- outputTableDescription(table_names[[3]])
  
  output[[table_names[[4]]]] <- outputTable(table_names[[4]])
  output[[paste0("Description",table_names[[4]])]] <- outputTableDescription(table_names[[4]])
  
  output[[table_names[[5]]]] <- outputTable(table_names[[5]])
  output[[paste0("Description",table_names[[5]])]] <- outputTableDescription(table_names[[5]])
  
  output[[table_names[[6]]]] <- outputTable(table_names[[6]])
  output[[paste0("Description",table_names[[6]])]] <- outputTableDescription(table_names[[6]])
  
  output[[table_names[[7]]]] <- outputTable(table_names[[7]])
  output[[paste0("Description",table_names[[7]])]] <- outputTableDescription(table_names[[7]])
  
  output[[table_names[[8]]]] <- outputTable(table_names[[8]])
  output[[paste0("Description",table_names[[8]])]] <- outputTableDescription(table_names[[8]])
  
  output[[table_names[[9]]]] <- outputTable(table_names[[9]])
  output[[paste0("Description",table_names[[9]])]] <- outputTableDescription(table_names[[9]])
  
  output[[table_names[[10]]]] <- outputTable(table_names[[10]])
  output[[paste0("Description",table_names[[10]])]] <- outputTableDescription(table_names[[10]])
  
  output[[table_names[[11]]]] <- outputTable(table_names[[11]])
  output[[paste0("Description",table_names[[11]])]] <- outputTableDescription(table_names[[11]])
  
  output[[table_names[[12]]]] <- outputTable(table_names[[12]])
  output[[paste0("Description",table_names[[12]])]] <- outputTableDescription(table_names[[12]])
  

  
}

shinyApp(ui, server)
