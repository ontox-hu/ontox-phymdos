source("helpers.R")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    uiOutput("mysidebar")
  ),
  dashboardBody(
    uiOutput("mycontent")
  )
  
)

server <- function(input, output, session) {
  
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "initial")
  })
  
  # Use reactive values when working with Shiny.
  subitems <- reactiveVal(value = table_names)
  
  # dynamic sidebar menu #
  output$mysidebar <- renderUI({
    sidebarMenu(id = "tabs",
                menuItem("Start", tabName = "initial", icon = icon("star"), selected = T),
                menuItem("Subs", id = "subs", tabName = "subs",  icon = icon("dashboard"), 
                         startExpanded = F,
                         lapply(subitems(), function(x) {
                           menuSubItem(x, tabName = paste0("sub_", x)) } )),
                menuItem("Setup", tabName = "setup")
    )
  })
  
  # dynamic content #
  output$mycontent <- renderUI({
    
    itemsSubs <- lapply(subitems(), function(x){
      tabItem(tabName = paste0("sub_", x), uiOutput(paste0("sub_", x)))
    })
    
    items <- c(
      list(
        tabItem(tabName = "initial",
                "Welcome on the initial page!"
        )
      ),
      
      itemsSubs,
      
      list(
        tabItem(tabName = "setup",
                
                selectInput("add_subitem", "Add subitem", choices = subitems()),
                actionButton("add", "Add table"),
                
                selectInput("rm_subitem", "Remove subitem", choices = subitems()),
                actionButton("rm", "Remove Table")
        )
      )
    )
    
    do.call(tabItems, items)
  })
  
  # dynamic content in the dynamic subitems #
  observe({ 
    lapply(subitems(), function(x){
      output[[paste0("sub_", x)]] <- renderUI ({
        list(fluidRow(
          box("hello"),
          #rHandsontableOutput(table_names["Reaction"], height = 200, width = 1000)
          #displayTabContent(table_names[[1]])
        )
        )
      })
      # output[[table_names[[1]]]] <- outputTable(table_names[[1]])
      # output[[paste0("Description",table_names[[1]])]] <- outputTableDescription(table_names[[1]])
      # 
      # output[[table_names[[2]]]] <- outputTable(table_names[[2]])
      # output[[paste0("Description",table_names[[2]])]] <- outputTableDescription(table_names[[2]])
      # 
      # output[[table_names[[3]]]] <- outputTable(table_names[[3]])
      # output[[paste0("Description",table_names[[3]])]] <- outputTableDescription(table_names[[3]])
      # 
      # output[[table_names[[4]]]] <- outputTable(table_names[[4]])
      # output[[paste0("Description",table_names[[4]])]] <- outputTableDescription(table_names[[4]])
      # 
      # output[[table_names[[5]]]] <- outputTable(table_names[[5]])
      # output[[paste0("Description",table_names[[5]])]] <- outputTableDescription(table_names[[5]])
      # 
      # output[[table_names[[6]]]] <- outputTable(table_names[[6]])
      # output[[paste0("Description",table_names[[6]])]] <- outputTableDescription(table_names[[6]])
      # 
      # output[[table_names[[7]]]] <- outputTable(table_names[[7]])
      # output[[paste0("Description",table_names[[7]])]] <- outputTableDescription(table_names[[7]])
      # 
      # output[[table_names[[8]]]] <- outputTable(table_names[[8]])
      # output[[paste0("Description",table_names[[8]])]] <- outputTableDescription(table_names[[8]])
      # 
      # output[[table_names[[9]]]] <- outputTable(table_names[[9]])
      # output[[paste0("Description",table_names[[9]])]] <- outputTableDescription(table_names[[9]])
      # 
      # output[[table_names[[10]]]] <- outputTable(table_names[[10]])
      # output[[paste0("Description",table_names[[10]])]] <- outputTableDescription(table_names[[10]])
      # 
      # output[[table_names[[11]]]] <- outputTable(table_names[[11]])
      # output[[paste0("Description",table_names[[11]])]] <- outputTableDescription(table_names[[11]])
      # 
      # output[[table_names[[12]]]] <- outputTable(table_names[[12]])
      # output[[paste0("Description",table_names[[12]])]] <- outputTableDescription(table_names[[12]])
    })
  })
  
  # add and remove tabs
  observeEvent(input$add, {
    req(input$add_subitem)
    
    s <- c(subitems(), input$add_subitem)
    subitems(s)
    
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  observeEvent(input$rm, {
    req(input$rm_subitem)
    
    s <- subitems()[-which(subitems() == input$rm_subitem)]
    subitems(s)
    
    updateTabItems(session, "tabs", selected = "setup")
  })
  
}

shinyApp(ui, server)