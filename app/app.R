# credit: dynamic table selection adapted from https://mgei.github.io/post/dynamic-shinydashboard/
source(
  "helpers.R"
)
source(
  "open_python.R"
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
  dashboardHeader(title = "ONTOX - Physiological Maps Data Entry Portal",
                                  titleWidth = 500),
  dashboardSidebar(
    sidebarMenuOutput("mysidebar")
  ),
  dashboardBody(
    tab_list_ui()
  )
)

# server
server <- function(input, output, session) {
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  ## render setup screen
  output$mysetup <- renderUI({
    bsCollapse(id = "homescreen", open = "Homescreen",
      # create home- choicescreen       
      bsCollapsePanel("Homescreen",
        htmlOutput("welcome"),
        actionButton("new_sbtab", "Create new SBtab"),
        actionButton("upload_sbtab", "Upload an SBtab object"),
        actionButton("upload_sbml", "Upload an SBML object")
      ),
      # upload screen for SBtab file
      bsCollapsePanel("Upload SBtab",
        fileInput("sbtabfile_in", "Upload SBtab file",
                  multiple = FALSE,
                  accept = c("text/tsv",
                             "text/tab-separated-values,text/plain",
                             ".tsv")),
        actionButton("set_sbtab", "Click here to continue (required)")
      ),
      bsCollapsePanel("Upload SBML",
        fileInput("sbmlfile_in", "Upload SBML file",
                  multiple = FALSE,
                  accept = c("text/xml",
                             "text/plain",
                             ".xml")),
        actionButton("set_sbml", "Click here to continue (required)")
      ),
      bsCollapsePanel("First setup",
        textInput("set_documentname", "Please name your document", placeholder = "Map name"),
        selectInput("sbtab_version", "Please enter which SBtab Version you need (1.0 default)", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        actionButton("set", "Save input")
      ),
      bsCollapsePanel("Save and download",
        htmlOutput("text_hot"),
        actionButton("save_hot", "Save table"),
        br(), br(), br(),
        htmlOutput("text_download"),
        downloadButton("download_tsv", "Download tsv"),
        downloadButton("download_xml", "Download xml")
      )
    )
  })
  
  # Render homescreen text
  output$welcome <- renderText({paste("<b>What would you like to do?</b>")})
  
  # Open "First setup" panel when "Create new SBtab" is selected
  observeEvent(input$new_sbtab, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
  # Open "Upload SBtab" panel when "Upload an SBtab object" is selected
  observeEvent(input$upload_sbtab, {
    updateCollapse(session, "homescreen", open = "Upload SBtab")
  })
  
  # Open "Upload SBML" panel when "Upload an SBML object" is selected
  observeEvent(input$upload_sbml, {
    updateCollapse(session, "homescreen", open = "Upload SBML")
  })
  
  # Open "First setup" panel after SBtab is uploaded
  observeEvent(input$set_sbtab, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
  # Open "First setup" panel after SBML is uploaded
  observeEvent(input$set_sbml, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
  # Open "configure map" panel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Save and download")
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # Head to save and download tab
  observeEvent(input$goto_download, {
  updateTabItems(session, "tabs", selected = "setup")
  })
    
  # Render text for setup page
  output$text_hot <- renderText({
    paste("<b>Press <i>Save table</i> to save your file before downloading it</b>")
  })
  
  output$text_download <- renderText({
    paste("<b>Download your file to .tsv or .xml format</b>")
  })
  
  # render select_tables
  output$mytables <- renderUI({
    tagList(
      selectInput("add_subitem", "Select table to add",
                  choices = local$choices),
      actionButton("add", "Add"),
      br(), br(),
      selectInput("rm_subitem", "Select table to remove",
                  choices = local$subitems$name),
      actionButton("rm", "Remove")
    )
  })
  
  # store dynamic tab list and dynamic contents
  local <- reactiveValues(
    empty_tabs = as.list(table_names),
    current_tabs = list(),
    subitems = data.frame(id = integer(), name = character()),
    choices = table_names
  )
  
  # dynamic sidebar menu #
  output$mysidebar <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Setup", tabName = "setup",
        icon = icon("gear"), selected = TRUE
      ),
      menuItem(
        "Select tables", tabName = "select_tables", 
        icon = icon("table")
      ),
      menuItem(
        "Tables", id = "subs", tabName = "subs", 
        icon = icon("database"), startExpanded = TRUE,
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
    id <- table_names[which(input$add_subitem == table_names)]
    # update empty/current tab lists
    local$empty_tabs <- local$empty_tabs[-which(local$empty_tabs == id)]
    local$current_tabs <- append(local$current_tabs, id)
    # tab name
    subitem <- input$add_subitem
    local$subitems <- rbind(local$subitems, 
                            data.frame(id = id, name = subitem))
    # remove name of table from choices 
    local$choices <- local$choices[local$choices!=subitem]
    updateTabItems(session, "tabs", selected = "select_tables")
    
    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", subitem)]] <- renderUI ({
      list(
        bsCollapsePanel("Select columns to include",
                        checkboxGroupInput(paste0(subitem, "_cols"),
                                           "Choose from:",
                                           choices = names(sbtab_tables_list[[subitem]]),
                                           selected = c("ReferenceDOI", "ID", "ReactionID"),
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
        actionButton("goto_download", "Click here to go to the download screen" ),
        br(), br(),
        bsCollapsePanel("Description of table elements",
                        DT::dataTableOutput(paste0("Description", subitem), 
                                            width = "100%")
        )
      )
    })
    
    # make reactive dataframes out of table choices
    df <- sbtab_tables_list[[subitem]]
    values <- reactiveValues(data = df)
    
    # save hot values to reactive dataframe
    observeEvent(input[[paste0(subitem, "_hot")]], {
      values$data <- hot_to_r(input[[paste0(subitem, "_hot")]])
    })
    
    # update dynamic content in the created table
    output[[paste0(subitem, "_hot")]] <- renderRHandsontable({
      rhandsontable(values$data, rowHeaders = NULL) %>%
        hot_cols(colWidths = 0.1) %>%
        hot_col(col = input[[paste0(subitem, "_cols")]], colWidths = "100%")
    })
      
    # output description table
    output[[paste0("Description", subitem)]] <- outputTableDescription(subitem)
    
    # write in table header
    tableheader <- 
      paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
      
    # write table header and columns to file
    observeEvent(input$save_hot, {
      write_lines(tableheader, file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(values$data), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines(" ", file = "physmap.tsv", append = TRUE)
      source_python("sbtab_to_sbml.py")
    })
    
    # 
    observeEvent({input$sbtabfile_in 
      input$set_sbtab}, {
        sbtabdata <- read_sbtab(input$sbtabfile_in)
    })
  })
  
  # set document header
  observeEvent({input$set
    input$save_hot}, {
      req(input$set_documentname)
      documentname_set <- 
        paste0('!!!SBtab Document="', input$set_documentname, '"') %>% as.character()
      write_lines(documentname_set, file = "physmap.tsv")
    })
  
  # remove a tab
  observeEvent(input$rm, {
    req(input$rm_subitem)
    req(length(local$empty_tabs) < 12)
    # add name of table from choices
    local$choices <- local$choices %>% c(paste(input$rm_subitem))
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
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # download tab content to .tsv and .xml
  output$download_tsv <- downloadHandler(
    filename = "physmap.tsv",
    content = function(file) {
      file.copy("physmap.tsv", file)
    })
  
  output$download_xml <- downloadHandler(
    filename = "physmap.xml",
    content = function(file) {
      file.copy("physmap.xml", file)
    })
}

shinyApp(ui, server)