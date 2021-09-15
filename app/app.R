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
  
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  # render setup
  output$mysetup <- renderUI({
    tagList(
        textInput("documentname", "Please name your document"),
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
    
    # dynamic content in the selected table
    output[[ paste0("sub_", id) ]] <- renderUI ({
      list(
        fluidRow(
          displayTabContent(table_names[which(table_names_df$name == subitem)])
        )
      )
    })
    
    # update dynamic content in the created table
     output[[table_names[which(table_names_df$name == subitem)]]] <- outputTable(table_names[which(table_names_df$name == subitem)])
     output[[paste0("Description", table_names[which(table_names_df$name == subitem)])]] <- outputTableDescription(table_names[which(table_names_df$name == subitem)])
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
  
  # format the tab content to be compatible with .xml conversion 
  observeEvent(input$documentname, {
    cat(paste0('!!!SBtab Document="', input$documentname, '"'), file = "physmap.tsv", sep = "\n")
    #Convert to R object
    x <- hot_to_r(
      for(is.element(table_names[which(table_names_df$name == subitem)], table_names_df$name))
      {paste0("input$",table_names_df$name[which(table_names_df$name == subitem)])}
      )
    # Convert column names to SBtab format
    colnames(x) <- paste0("!", colnames(x))
    # Write in table header
    cat(paste0('!!SBtab TableID="t_',subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$documentname, '"',' TableType="',subitem, '"',' TableName="',subitem, '"'), file = "physmap.tsv", sep = "\n", append = T)
    # Write in table
    cat(write.table(x, row.names=FALSE, na = "", quote=F, sep="\t"), file = "physmap.tsv", sep = "\n", append = T)
  })
  
  # download tab content to .tsv
  output$download <- downloadHandler(
    filename = "physmap.tsv",
    content = function(file) {
      file.copy("physmap.tsv", file) 
    })
}

shinyApp(ui, server)