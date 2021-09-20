# credit: adapted from https://mgei.github.io/post/dynamic-shinydashboard/
source(
  "helpers.R"
)

options(stringsAsFactors = FALSE)

# static tab list
tab_list_ui <- function() {
  # concatenate static tabs
  items <- c(
    list(
      tabItem(
        tabName = "setup",
        uiOutput("mysetup")
      )
    ),
    lapply(1:12, function(id) {
      tabItem(
        tabName = paste0("tab_", id), 
        uiOutput(paste0("sub_", id))
      )
    })
  )
  # render
  do.call(tabItems, items)
}

# dynamic sub menu
update_submenu <- function(local) {
  lapply(split(local$subitems, seq(nrow(local$subitems))), function(x) {
    menuSubItem(x$name, tabName = paste0("tab_", x$id))
  })
}

# ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenuOutput("mysidebar")
  ),
  dashboardBody(
    tab_list_ui()
  )
)

# server
server <- function(input, output, session) {
  observeEvent(input$documentname, {
  # format a file containing tab content compatible with .xml conversion 
  document <- write_lines(paste0('!!!SBtab Document="', input$documentname, '"'), file = "physmap.tsv", sep = "\n")
  })
  
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  # render setup
  output$mysetup <- renderUI({
    tagList(
        textInput("documentname", "Please name your document", placeholder = "Map name"),
        selectInput("sbtab_version", "Which SBtab Version do you need (1.0 default)?", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        selectInput("add_subitem", "Add subitem",
                    choices = table_names),
        actionButton("add", "add!"),
        selectInput("rm_subitem", "Remove subitem",
                    choices = local$subitems$name),
        actionButton("rm", "remove!"),
        br(),
        br(),
        downloadButton("download", "Download tsv")
    )
  })
  
  # store dynamic tab list and dynamic contents
  local <- reactiveValues(
    empty_tabs = as.list(1:12),
    current_tabs = list(),
    subitems = data.frame(id = integer(), name = character())
  )
  
  # dynamic sidebar menu #
  output$mysidebar <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Setup", tabName = "setup", 
        icon = icon("gear"), selected = T
      ),
      menuItem(
        "Subs", id = "subs", tabName = "subs", 
        icon = icon("dashboard"), startExpanded = T,
        update_submenu(local)
      )
    )
  })
  
  # debugging
  observe({
    print(paste0("current tabs = ", 
                 paste0(unlist(local$current_tabs), collapse = " ")))
    print(paste0("empty tabs = ", 
                 paste0(unlist(local$empty_tabs), collapse = " ")))
  })
  
  # add a tab
  observeEvent(input$add, {
    req(input$add_subitem)
    req(length(local$empty_tabs) > 0)
    # id of next tab to fill
    id <- min(unlist(local$empty_tabs))
    # update empty/current tab lists
    local$empty_tabs <- local$empty_tabs[-which(local$empty_tabs == id)]
    local$current_tabs <- append(local$current_tabs, id)
    # tab name
    subitem <- input$add_subitem
    local$subitems <- rbind(local$subitems, 
                            data.frame(id = id, name = subitem))
    updateTabItems(session, "tabs", selected = "setup")
    
    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", id) ]] <- renderUI ({
      list(
        tabItem(
          tabName = subitem,
          fluidRow(
            rHandsontableOutput(subitem, height = 400, width = "100%")
          ),
          fluidRow(
            column(
              10,
              DT::dataTableOutput(
                paste0(
                  "Description", 
                  subitem), 
                width = "100%"), 
              offset = 0)
          )
        )
      )
    })
    
    # make table a reactive dataframe
    df <- sbtab_tables_list[[subitem]]
    values <- reactiveValues(data = df)
    
    # save hot values to reactive dataframe
    observeEvent(input[[subitem]], {
      values$data <- hot_to_r(input[[subitem]])
      
      # Convert column names to SBtab format
      tableValues <- values$data
      colnames(tableValues) <- paste0("!", colnames(tableValues))
      # Write in table header
      tableitem <- write_lines(paste(paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"'), 
                                   write.table(tableValues, row.names=FALSE, na = "", quote=F, sep="\t", file = "physmap.tsv"), sep = "\n"), file = "physmap.tsv")
      # Write in table
    #  subitem <- write.table(tableValues, row.names=FALSE, na = "", quote=F, sep="\t", file = "physmap.tsv", append = T)
      fullDocument <- write_lines(paste(document, tableitem))
      })
    
    # update dynamic content in the created table
    output[[subitem]] <- renderRHandsontable({rhandsontable(values$data, rowHeaders = NULL) })
    output[[paste0("Description", subitem)]] <- outputTableDescription(subitem)
    
  })
  
  # remove a tab
  observeEvent(input$rm, {
    req(input$rm_subitem)
    req(length(local$empty_tabs) < 12)
    # id of tab to fill
    subitem_ind <- which(local$subitems$name == input$rm_subitem)
    subitem <- local$subitems[subitem_ind,]
    # update empty/current tab lists
    local$empty_tabs <- append(local$empty_tabs, subitem$id)
    local$current_tabs <- local$current_tabs[-which(local$current_tabs == subitem$id)]
    # reset deleted tab
    shinyjs::reset(paste0("sub_", subitem$id))
    # tab name
    local$subitems <- local$subitems[-subitem_ind,]
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  
  # # format the tab content to be compatible with .xml conversion
  # observeEvent(input$values$data, {
  #   subitem <- input$add_subitem
  #   cat(paste0('!!!SBtab Document="', input$documentname, '"'), file = "physmap.tsv", sep = "\n")
  #   tableValues <- values$data
  #   # Convert column names to SBtab format
  #   colnames(values$data) <- paste0("!", colnames(values$data))
  #   # Write in table header
  #   cat(paste0('!!SBtab TableID="t_',input$add_subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$documentname, '"',' TableType="',input$add_subitem, '"',' TableName="',input$add_subitem, '"'), file = "physmap.tsv", sep = "\n", append = T)
  #   # Write in table
  #   write.table(tableValues, row.names=FALSE, na = "", quote=F, sep="\t", file = "test.tsv", append = T)
  #   
  #   cat(values$data, file = "physmap.tsv", sep = "\n", append = T)
  #   cat(paste0(input$add_subitem, input$Reaction, "Hello2"), file = "physmap.tsv", sep = "\n", append = T)
  # })
    
  
  
  # download tab content to .tsv
  output$download <- downloadHandler(
    filename = "physmap.tsv",
    content = function(file) {
      write_lines(paste(document, tableitem))
      #file.copy("physmap.tsv", file)
    })
}

shinyApp(ui, server)