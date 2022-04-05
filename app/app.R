# credit: dynamic table selection adapted from https://mgei.github.io/post/dynamic-shinydashboard/
## source files containing supporting functions and open python
source(
  "R/helpers.R"
)
source(
  "R/open_python.R"
)

options(stringsAsFactors = FALSE)

## ui
ui <- dashboardPage(title = "Phymdos", 
  dashboardHeader(
                  titleWidth = 0,
                  tags$li(a(onclick = "onclick =window.open('https://github.com/ontox-hu/ontox-phymdos')",
                            href = NULL,
                            icon("github"),
                            title = "GitHub",
                            style = "cursor: pointer;"
                  ),
                  class = "dropdown"
                  )
                  ),
  dashboardSidebar(
    tags$img(src='phymdos_logo.png',height='200',width='200', style='text-align: center;'),
    width = 200,
    sidebarMenuOutput("mysidebar")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
      ),
    tab_list_ui(),
    useShinyjs()
  )
)

## server
server <- function(input, output, session) {
  # dynamic sidebar menu 
  output$mysidebar <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Setup", tabName = "setup",
        icon = icon("cog"), selected = TRUE
      ),
      menuItem(
        "Select tables", tabName = "select_tables", 
        icon = icon("table")
      ),
      menuItem(
        "Tables", id = "subs", tabName = "subs", 
        icon = icon("database"), startExpanded = TRUE,
        update_submenu(local)
      ),
      menuItem(
        "Help", tabName = "help",
        icon = icon("question")
      ),
      menuItem(
        "Info and contact", tabName = "info",
        icon = icon("info")
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
  
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  # store dynamic tab list and dynamic contents to use in app
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
    sbtabfile = list(),
    # create vectors for minerva upload status
    minerva_status = list("Status: empty", "Progress: empty", 0),
    minerva_progress = " "
  )
  
  
  ## render setup screen
  output$mysetup <- renderUI({
    bsCollapse(id = "homescreen", open = "Homescreen",
      # create home- choicescreen       
      bsCollapsePanel("Homescreen",
        textInput("set_documentname", "Please name your document", placeholder = "Documentname"),
        htmlOutput("welcome"),
        actionButton("new_sbtab", "Create new SBtab"),
        actionButton("upload_sbtab", "Upload an SBtab object"),
        actionButton("upload_sbml", "Upload an SBML object")
      ),
      # upload SBtab file page
      bsCollapsePanel("Upload SBtab",
        fileInput("sbtabfile_in", "Upload SBtab file (.tsv)",
                  multiple = FALSE,
                  accept = c("text/tsv",
                             "text/tab-separated-values,text/plain",
                             ".tsv")),
        actionButton("set_sbtab", "Click here to continue (required)")
      ),
      # upload SBML file page
      bsCollapsePanel("Upload SBML",
        fileInput("sbmlfile_in", "Upload SBML file (.xml)",
                  multiple = FALSE,
                  accept = c("text/xml",
                             "text/plain",
                             ".xml")),
        actionButton("set_sbml", "Click here to continue (required)")
      ),
      # download page for SBtab/SBML
      bsCollapsePanel("Save and download",
        htmlOutput("text_download"),
        downloadButton("download_tsv", "Download SBtab (.tsv)"),
        downloadButton("download_xml", "Download SBML (.xml)"),
        br(),
        br(),
        htmlOutput("text_minerva"),
        actionButton("open_minerva", 
                     "Open MINERVA annotated"), 
        actionButton("open_minerva_fast", 
                     "Open MINERVA unannotated"),
        htmlOutput("status")
      )
    )
  })
  
  # Render homescreen text
  output$welcome <- renderText({paste("<b>What would you like to do?</b>")})
  
  # Open "select_tables" panel when "Create new SBtab" is selected
  observeEvent(input$new_sbtab, {
    updateTabItems(session, "tabs", selected = "select_tables")
    updateCollapse(session, "homescreen", open = "Save and download")
  })
  
  # Open "Upload SBtab" panel when "Upload an SBtab object" is selected
  observeEvent(input$upload_sbtab, {
    updateCollapse(session, "homescreen", open = "Upload SBtab")
  })
  
  # Open "Upload SBML" panel when "Upload an SBML object" is selected
  observeEvent(input$upload_sbml, {
    updateCollapse(session, "homescreen", open = "Upload SBML")
  })
  
  # read input sbtab to dashboard
  observeEvent(input$sbtabfile_in, {
    # debug message
    debug_msg("Uploading SBtabfile")
    # return message when uploading wrong file
    tryCatch({
      # read the sbtab into list of tables
      local$sbtabfile <- read_sbtab(input$sbtabfile_in$datapath)
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
      # debug message
      debug_msg("SBtabfile uploaded succesfully")
    }, error = function(e){
      shinyalert(
        title = "Error",
        text = e$message,
        size = "l", 
        closeOnClickOutside = TRUE,
        type = "warning",
        showConfirmButton = TRUE,
        confirmButtonText = "Continue",
        confirmButtonCol = "#1fa9ff",
        animation = FALSE
      )
      # debug message
      debug_msg(paste("SBtabfile uploaded error:", e$message))
    })
  })
  
  # read input sbml to dashboard
  observeEvent(input$sbmlfile_in, {
    # debug message
    debug_msg("Uploading SBMLfile")
    # return message when uploading wrong file
    tryCatch({
      # convert sbml to sbtab and read into list of tables
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
      # debug message
      debug_msg("SBMLfile uploaded succesfully")
    }, error = function(e){
      shinyalert(
        title = "Error",
        text = e$message,
        size = "l", 
        closeOnClickOutside = TRUE,
        type = "warning",
        showConfirmButton = TRUE,
        confirmButtonText = "Continue",
        confirmButtonCol = "#1fa9ff",
        animation = FALSE
      )
      # debug message
      debug_msg(paste("SBML uploaded error:", e$message))
    })
  })  
  
  # open table tabs from uploaded files
  observeEvent(input$set_sbtab|input$set_sbml, {
    req(input$set_sbtab|input$set_sbml)
    # debug message
    debug_msg("Opening tables")
    # open tabs included in sbtab in the dashboard
    lapply(names(local$sbtabfile), function(table){
      # update empty/current tab lists if the table is not open yet
      if(!(table %in% local$current_tabs)){
        local$empty_tabs <- local$empty_tabs[local$empty_tabs!=table]
        local$current_tabs <- append(local$current_tabs, table)
        # tab name
        local$subitems <- rbind(local$subitems,
                                data.frame(id = table, name = table)
                                )
        # write table header for file
        local$headers <- append(local$headers,
                                paste0('!!SBtab TableID="t_', table, '"', ' SBtabVersion="1.0"', ' Document="', input$set_documentname, '"',' TableType="', table, '"',' TableName="', table, '"')
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
        # output description table
        output[[paste0("Description", table)]] <- outputTableDescription(table)
        # Head to save and download tab
        observeEvent(input[[paste0("goto_download_", table)]], {
          updateTabItems(session, "tabs", selected = "setup")
          updateCollapse(session, "homescreen", open = "Save and download")
        })
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
    
    # open "select_tables" panel after SBtab or SBML is uploaded and the continue button is pressed
    updateTabItems(session, "tabs", selected = "select_tables")
    updateCollapse(session, "homescreen", open = "Save and download")
    # debug message
    debug_msg("Tables opened succesfully")
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

  ## add a tab
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
                            data.frame(id = id, name = subitem)
                            )
    # write table header for file
    local$headers <- append(local$headers,
                            paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="1.0"', ' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
                            )
    names(local$headers) <- local$current_tabs
    # remove name of table from choices
    local$choices <- local$choices[local$choices!=subitem]
    updateTabItems(session, "tabs", selected = "select_tables")
    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", subitem)]] <- renderUI ({
      add_tableUI(subitem)
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
    # head to save and download tab
    observeEvent(input[[paste0("goto_download_", subitem)]], {
      updateTabItems(session, "tabs", selected = "setup")
      updateCollapse(session, "homescreen", open = "Save and download")
    })
    # output description table
    output[[paste0("Description", subitem)]] <- outputTableDescription(subitem)
  })
  
  ## write tsv and xml documents actively
  observeEvent(local$data, {
    # set documentname header in SBtab output file
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
    # write tables into SBtab output file
    for(table in local$current_tabs){
      write_lines(local$headers[[table]], file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(local$data[[table]]), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines("", file = "physmap.tsv", append = TRUE)
    }
    # write SBML output file
    tryCatch({
      sbtab_to_sbml("physmap.tsv")
      },
      # make it so that sbtab conversion errors don't crash the app 
      # (incomplete sbtab document will cause the .py script to return error)
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
    # add name of table back to choices
    local$choices <- local$choices %>% c(paste(input$rm_subitem))
    # id of tab to fill
    subitem_ind <- which(local$subitems$name == input$rm_subitem)
    subitem <- local$subitems[subitem_ind,]
    # update empty/current tab lists
    local$empty_tabs <- append(local$empty_tabs, subitem$id)
    local$current_tabs <- local$current_tabs[-which(local$current_tabs == subitem$id)]
    # # reset deleted tab and tab content
    # shinyjs::reset(paste0("sub_", subitem$id))
    # local$data[which(names(local$data) == subitem$id)] <- sbtab_tables_list[which(names(sbtab_tables_list) == subitem$id)]
    # print(local$data[which(names(local$data) == subitem$id)])
    # updateCheckboxGroupInput(session, paste0(input$rm_subitem, "_cols"),
    #                          selected = c("ReferenceDOI",
    #                                       "ID",
    #                                       "ReactionID")
    #                          )
    # tab name
    local$subitems <- local$subitems[-subitem_ind,]
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # render text in download tab
  output$text_download <- renderText({
    paste("<b>Download your file to .tsv or .xml format</b>")
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
      write_file(sbml_string, file)
    })
  
  # render text for minerva in download tab
  output$text_minerva <- renderText({
    paste("<b>Opening Minerva with annotation may take significantly longer than without annotation</b>")
  })
  
  # open minerva on click annotated
  observeEvent(input$open_minerva, {
    # debug message
    debug_msg("Generating MINERVA map")
    showNotification("Please wait a few moments for the page to load")
    # source minerva script
    source_python("py/minerva_upload.py")
    # Get minerva status for status counter
    local$minerva_status <- get_status()
    while(local$minerva_status[[3]] < 100 ){
      local$minerva_status <- get_status()
      local$minerva_progress <- paste("<b>", "MINERVA map status:", local$minerva_status[[1]], local$minerva_status[[2]], "</b>", sep = "<br/>")
      # update text output 
      html(id = "status", local$minerva_progress)
      # display status in console
      cat(paste(local$minerva_status[[1]], local$minerva_status[[2]], "", sep = "\n"))
      Sys.sleep(1)
      if(local$minerva_status[[3]] > 100){
        local$minerva_status <- as.list(c("Status: ok", "Progress: 100.0%", 100))
        # display status in console
        cat(paste(local$minerva_status[[1]], local$minerva_status[[2]], "", sep = "\n"))
      }
    }
    # open web page
    runjs(
      paste0("$('<a>', {href: 'http://145.38.204.52:8080/minerva/index.xhtml?id=",
             minerva_long,
             "', target: '_blank'})[0].click();"
      )
    )
    # debug message
    debug_msg("MINERVA opened succesfully")
  })
  
  # open minerva on click unannotated
  observeEvent(input$open_minerva_fast, {
    # debug message
    debug_msg("Generating MINERVA map")
    showNotification("Please wait a few seconds for the page to load")
    # source minerva script
    source_python("py/minerva_upload_short.py")
    # Get minerva status for status counter
    local$minerva_status <- get_status()
    while(local$minerva_status[[3]] < 100 ){
      local$minerva_status <- get_status()
      local$minerva_progress <- paste("<b>", "MINERVA map status:", local$minerva_status[[1]], local$minerva_status[[2]], "</b>", sep = "<br/>")
      # update text output 
      html(id = "status", local$minerva_progress)
      # display status in console
      cat(paste(local$minerva_status[[1]], local$minerva_status[[2]], "", sep = "\n"))
      Sys.sleep(1)
      if(local$minerva_status[[3]] > 100){
        local$minerva_status <- as.list(c("Status: ok", "Progress: 100.0%", 100))
        # display status in console
        cat(paste(local$minerva_status[[1]], local$minerva_status[[2]], "", sep = "\n"))
      }
    }
    # open web page
    runjs(
      paste0("$('<a>', {href: 'http://145.38.204.52:8080/minerva/index.xhtml?id=",
             minerva_short,
             "', target: '_blank'})[0].click();"
      )
    )
    # debug message
    debug_msg("MINERVA opened succesfully")
  })
  
  ## render minerva status text 
  output$status <- renderText({
    HTML(paste("<b>", "MINERVA map status:", local$minerva_status[[1]], local$minerva_status[[2]], "</b>", sep = "<br/>"))
  })
  
  ## render help screen
  output$myhelp <- renderUI({
    includeMarkdown("documentation/README_copy.md")
  })
  
  ## render info screen
  output$myinfo <- renderUI({
    includeMarkdown("documentation/contact_info.md")
  })

}
shinyApp(ui, server)#,
         # # clear clutter 
         # onStart = function() {
         #   print("Launching server")
         #   onStop(function() {
         #     print("Shutting down, potential leftover warnings:")
         #     })
         #   })

