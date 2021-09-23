# credit: dynamic table selection adapted from https://mgei.github.io/post/dynamic-shinydashboard/
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
    list(
      tabItem(
        tabName = "select_tables",
        uiOutput("mytables")
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
  
  # render setup
  output$mysetup <- renderUI({
    bsCollapse(id = "homescreen", open = "Set documentname",
      bsCollapsePanel("Set documentname",
        textInput("set_documentname", "Please name your document", placeholder = "Map name"),
        actionButton("set", "Set")
      ),
      bsCollapsePanel("Configure map",
        selectInput("sbtab_version", "Which SBtab Version do you need (1.0 default)?", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        br(), br(),
        actionButton("save_hot", "Save table"),
        br(), br(),
        downloadButton("download", "Download tsv")
      )
    )
  })
  
  # Open "configure map" pannel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Configure map")
  })
  
  # render select_tables
  output$mytables <- renderUI({
    tagList(
      selectInput("add_subitem", "Add subitem",
                  choices = table_names),
      actionButton("add", "Add"),
      selectInput("rm_subitem", "Remove subitem",
                  choices = local$subitems$name),
      actionButton("rm", "Remove")
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
        icon = icon("gear"), selected = TRUE
      ),
      menuItem(
        "Select tables", tabName = "select_tables", 
        icon = icon("columns")
      ),
      menuItem(
        "Subs", id = "subs", tabName = "subs", 
        icon = icon("dashboard"), startExpanded = TRUE,
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
  
  # set document header
  observeEvent({input$set
    input$save_hot}, {
    req(input$set_documentname)
    documentname_set <- 
      paste0('!!!SBtab Document="', input$set_documentname, '"') %>% as.character()
    write_lines(documentname_set, file = "physmap.tsv")
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
    updateTabItems(session, "tabs", selected = "select_tables")
    
    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", id) ]] <- renderUI ({
      list(
        bsCollapsePanel("Select columns to include",
          checkboxGroupInput(paste0(subitem, "_cols"),
                             "Choose from:",
                             choices = names(sbtab_tables_list[[subitem]]),
                             selected = c("ReferenceDOI", "ID"),
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
            )
            ),
          bsCollapsePanel("Description of table elements",
              DT::dataTableOutput(paste0("Description", subitem), 
                                  width = "100%")
          )
        )
      )
    })
    
    # make table a reactive dataframe
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
    
    # Write in table header
    tableheader <- 
      paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
      
    # Write table header and columns to file
    observeEvent(input$save_hot, {
      #values$data <- set_cols(values$data)
      write_lines(tableheader, file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(values$data), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines(" ", file = "physmap.tsv", append = TRUE)
      })
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
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # download tab content to .tsv
  output$download <- downloadHandler(
    filename = "physmap.tsv",
    content = function(file) {
      file.copy("physmap.tsv", file)
    })
}

shinyApp(ui, server)