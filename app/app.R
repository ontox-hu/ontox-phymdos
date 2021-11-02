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
  dashboardHeader(title = tags$p(tags$a(href='https://ontox-project.eu/',
                                 tags$img(src='ontox_logo.png',height='40',width='60')),
                                 " - Physiological Maps Data Entry Portal"), 
                  titleWidth = 500),
  dashboardSidebar(
    sidebarMenuOutput("mysidebar")
  ),
  dashboardBody(
    tab_list_ui(),
    useShinyjs()
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
        textInput("set_documentname", "Please name your document", placeholder = "Documentname"),
        selectInput("sbtab_version", "Please enter which SBtab Version you need (1.0 default)", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        actionButton("set", "Save input")
      ),
      bsCollapsePanel("Save and download",
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
  
  # Open "configure map" panel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Save and download")
    updateTabItems(session, "tabs", selected = "select_tables")
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
    choices = table_names,
    # create empty list for table headers (for exporting)
    headers = list(),
    # make reactive dataframes out of table choices
    data = sbtab_tables_list,
    # create empty list for data upload
    sbtabfile = list()
  )
  
  # read input sbtab to dashboard
  observeEvent(input$sbtabfile_in, {
    local$sbtabfile <- suppressWarnings(read_sbtab(input$sbtabfile_in$datapath))
    # print names of tables in the file to console
    print(paste("File", paste0("'",input$sbtabfile_in$name, "'"), "contains tabs:"))
    print(names(local$sbtabfile))
    local$data[names(local$sbtabfile)] <- lapply(names(local$sbtabfile), function(name){
      # make sure all columns start with uppercase letter
      colnames(local$sbtabfile[[name]]) <- 
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
             colnames(local$sbtabfile[[name]]),
             perl = TRUE)
      local$data[[name]] <- add_row(local$data[[name]], local$sbtabfile[[name]], .after = 0)
    })
  })
  
  # read input sbml to dashboard
  observeEvent(input$sbmlfile_in, {
    sbml_to_sbtab(input$sbmlfile_in$datapath)
    local$sbtabfile <- suppressWarnings(read_sbtab(sbtab_string))
    # print names of tables in the file to console
    print(paste("File", paste0("'",input$sbmlfile_in$name, "'"), "contains tabs:"))
    print(names(local$sbtabfile))
    local$data[names(local$sbtabfile)] <- lapply(names(local$sbtabfile), function(name){
      # make sure all columns start with uppercase letter
      colnames(local$sbtabfile[[name]]) <- 
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
             colnames(local$sbtabfile[[name]]),
             perl = TRUE)
      local$data[[name]] <- add_row(local$data[[name]], local$sbtabfile[[name]], .after = 0)
    })
  })  
  
  # open table tabs from uploaded files
  observeEvent(input$set_sbtab|input$set_sbml, {
    req(input$set_sbtab|input$set_sbml)
    # open tabs included in sbtab in the dashboard
    lapply(names(local$sbtabfile), function(table){
      # update empty/current tab lists if the table is not open yet
      if(!(table %in% local$current_tabs)){
        local$empty_tabs <- local$empty_tabs[local$empty_tabs!=table]
        local$current_tabs <- append(local$current_tabs, table)

        # tab name
        local$subitems <- rbind(local$subitems,
                                data.frame(id = table, name = table))

        # write table header for file
        local$headers <- append(local$headers,
                                paste0('!!SBtab TableID="t_', table, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', table, '"',' TableName="', table, '"')
        )

        # remove name of table from choices
        local$choices <- local$choices[local$choices!=table]

        # render dynamic table and description corresponding to tab name
        output[[paste0("sub_", table)]] <- renderUI({
          upload_tableUI(table, local$sbtabfile)
        })

        # save hot values to reactive dataframe
        observeEvent(input[[paste0(table, "_hot")]], {
          local$data[[table]] <- hot_to_r(input[[paste0(table, "_hot")]])
        })

        # update dynamic content in the created table
        output[[paste0(table, "_hot")]] <- renderRHandsontable({
          rhandsontable(local$data[[table]], rowHeaders = NULL) %>%
            hot_cols(colWidths = 0.1) %>%
            hot_col(col = input[[paste0(table, "_cols")]], colWidths = "100%")
        })

        # Head to save and download tab
        observeEvent(input[[paste0("goto_download_", table)]], {
          updateTabItems(session, "tabs", selected = "setup")
          updateCollapse(session, "homescreen", open = "Save and download")
        })

        # output description table
        output[[paste0("Description", table)]] <- outputTableDescription(table)
      }

      # If table was opened previously, open filled columns
      updateCheckboxGroupInput(session, paste0(table, "_cols"),
                               selected = c("ReferenceDOI",
                                            "ID",
                                            "ReactionID",
                                            names(local$sbtabfile[[table]][which(local$sbtabfile[[table]][1,] != "")]))
      )
    })
    names(local$headers) <- local$current_tabs

    # open "First setup" panel after SBtab or SBML is uploaded and the continue button is pressed
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
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

    # write table header for file
    local$headers <- append(local$headers,
                            paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
                            )
    names(local$headers) <- local$current_tabs

    # remove name of table from choices
    local$choices <- local$choices[local$choices!=subitem]
    updateTabItems(session, "tabs", selected = "select_tables")

    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", subitem)]] <- renderUI ({
      upload_tableUI(subitem)
    })
    
    # save hot values to reactive dataframe
    observeEvent(input[[paste0(subitem, "_hot")]], {
      local$data[[subitem]] <- hot_to_r(input[[paste0(subitem, "_hot")]])
    })

    # update dynamic content in the created table
    output[[paste0(subitem, "_hot")]] <- renderRHandsontable({
      rhandsontable(local$data[[subitem]], rowHeaders = NULL) %>%
        hot_cols(colWidths = 0.1) %>%
        hot_col(col = input[[paste0(subitem, "_cols")]], colWidths = "100%")
    })
    
    # Head to save and download tab
    observeEvent(input[[paste0("goto_download_", subitem)]], {
      updateTabItems(session, "tabs", selected = "setup")
      updateCollapse(session, "homescreen", open = "Save and download")
    })

    # output description table
    output[[paste0("Description", subitem)]] <- outputTableDescription(subitem)
  })
  
  # write tsv and xml documents reactively
  observeEvent(local$data, {
    documentname_set <-
      paste0('!!!SBtab Document="', 
             if(is_empty(input$set_documentname)){
               "Documentname"
               }else{
                 input$set_documentname
                 }, 
             '"') %>% 
      as.character()
    write_lines(documentname_set, file = "physmap.tsv")
    for(table in local$current_tabs){
      write_lines(local$headers[[table]], file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(local$data[[table]]), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines(" ", file = "physmap.tsv", append = TRUE)
    }
    tryCatch({
      sbtab_to_sbml("physmap.tsv")
      },
      warning = function(warn){
        print(warn)
      },
      error = function(err){
        print(err)
      })
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
      write_file(sbml, file)
    })
}

shinyApp(ui, server)