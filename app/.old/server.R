##################################################
## server.R
## Server part of Dashboard C.elegans Shiny app
## Marc A.T. Teunis, PhD
## January 2021
################################################


server <- function(input, output) {
  
  ## create reactive dataframe as source
  data_clean <- reactive({
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload an XLSX file"))
    
    readxl::read_excel(
      file$datapath,
      col_names = input$header,
      sheet = input$delim,
      skip = input$skip
    ) %>%
      prepare_data_shiny(
        df = ., 
        control = input$control,
        conc_as_categorical = input$conc_as_categorical)
    
    
  })

  ############# Raw Data table ##############################################
  
  output$RawData <- renderTable({
    
    data_clean()
    
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # data <- readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$delim,
    #   skip = input$skip
    # )
    # data
  })
  
  ############## Missingness plot  - Data Overview panel ######################
  
  output$plot_missing <- renderPlot({
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$delim,
    #   skip = input$skip
    # ) %>%
    
    data_clean() %>%
      naniar::vis_miss()
    
  })
  
  ################ Facets plot Data Overview - panel #####################
  
  output$DoseResponsePlot <- renderPlot({
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$sheet_name,
    #   skip = input$skip
    # ) %>%
    #   prepare_data_shiny(
    #     df = ., 
    #     control = input$control,
    #     conc_as_categorical = input$conc_as_categorical) -> data_clean
    
    if(input$conc_as_categorical == FALSE){
      
      data_clean() %>% 
        ggplot(aes(
          x = (comp_concentration + 0.0001) %>% as.numeric %>% log10,
          y = counts_norm
        )) +
        geom_point(aes(colour = comp_name),
                   position = "jitter",
                   show.legend = FALSE,
                   size = 2) +
        #     toolboxr::rotate_axis_labels("x", 45) +
        facet_wrap( ~ comp_name, ncol = 3) +
        xlab("Log10(Conc.)") +
        theme_bw() +
        xlab("Compound Concentration") +
        ylab("Normalized counts")
      
    } else {
      
      
      data_clean() %>% 
        ggplot(aes(
          x = comp_concentration,
          y = counts_norm
        )) +
        geom_point(aes(colour = comp_name),
                   position = "jitter",
                   show.legend = FALSE) +
        #     toolboxr::rotate_axis_labels("x", 45) +
        facet_wrap( ~ comp_name, ncol = 3) +
        xlab("Conc.)") +
        theme_bw() +
        xlab("Compound Concentration") +
        ylab("Normalized counts")
      
      
    }
    
    
  })
  
  ############# All data plot Data Overview panel #############################
  
  output$DoseResponseOverview <- renderPlot({
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$sheet_name,
    #   skip = input$skip
    # ) %>%
    #   prepare_data_shiny(
    #     df = ., 
    #     control = input$control,
    #     conc_as_categorical = input$conc_as_categorical) -> data_clean
    
    
    positive_control_df <- data_clean() %>%
      dplyr::filter(exp_type == "controlPositive")
    
    mean_positive_control <- mean(positive_control_df$counts_norm)
    
    if(input$conc_as_categorical == TRUE){
      
      data_clean() %>%
        ggplot(aes(
          x = comp_concentration,
          y = counts_norm)) +
        geom_point(
          aes(
            colour = comp_name,
            shape = exp_type
          ), 
          position = "jitter",       
          show.legend = TRUE,
          size = 4) +
        xlab("Concentration") +
        ylab("Normalized counts") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) 
      #+
      #  theme_bw() +
      #  facet_wrap(~ comp_name, ncol = 3)
      
    } else { 
      
      data_clean() %>%
        ggplot(aes(
          x = (comp_concentration + 0.0001) %>% as.numeric %>% log10,
          y = counts_norm
        )) +
        geom_point(aes(colour = comp_name,
                       shape = exp_type),
                   position = "jitter",
                   size = 4) +
        xlab("Log10(compound concentration)") +
        geom_hline(
          yintercept = mean_positive_control,
          linetype = "dashed",
          colour = "darkred",
          size = 1
        ) +
        xlab("Concentration") +
        ylab("Normalized counts") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45)) 
    }
    
    
  })
  
  
  ############# Dosis effect curve {drc} panel ###############################
  
  output$DoseResponseCurve <- renderPlot({
    
#     file <- input$file1
#     ext <- tools::file_ext(file$datapath)
#     
#     req(file)
#     validate(need(ext == "xlsx", "Please upload an XLSX file"))
# ## to debug run from here:    
#     readxl::read_excel(
#       file$datapath,
#       col_names = input$header,
#       sheet = input$sheet_name,
#       skip = input$skip
#     ) %>%
#       prepare_data_shiny(
#         df = ., 
#         control = input$control,
#         conc_as_categorical = input$conc_as_categorical
#       ) -> data_clean
    
    test_compounds <- data_clean() 
    
    
    test_compounds <- test_compounds  %>%
      dplyr::filter(exp_type == "experiment")
    test_compounds <- test_compounds$comp_name %>% unique()
    
    negative_control <- data_clean() 
    
    negative_control <- negative_control %>%
      dplyr::filter(exp_type == input$control)
    
    negative_control <- negative_control$comp_name %>% unique()
    
   # if(length(test_compounds) == 1){
    
  #  data_clean <- data_clean
  #  } else {
      
    data_clean_drc <- data_clean() 
    
    data_clean_drc <- data_clean_drc %>%
      mutate(comp_name = ifelse(comp_name == negative_control,
                                test_compounds[1],
                                comp_name))
   # }
    
    if(input$control == "controlVehicleA"){
      
      data_clean_drc <- data_clean_drc %>%
        dplyr::filter(exp_type != "controlPositive",
                      exp_type != "controlNegative")
      
    } 
    
    if(input$control == "controlNegative"){
      
      data_clean_drc <- data_clean_drc %>%
        dplyr::filter(exp_type != "controlPositive")
      
    } 
    
    #model <- list(get(input$drc_model))
    
    
   if(length(test_compounds) == 1){
   
     dose_effect <- drc::drm(
        data = data_clean_drc,
        counts_norm ~ comp_concentration,
        #  curveid = comp_name,
        fct = LL.3()
      ) 
     
     plot(dose_effect, type = "all") 
      
   } else {

      dose_effect <- drc::drm(
        data = data_clean_drc,
        counts_norm ~ comp_concentration,
        curveid = comp_name,
        fct = LL.3(),
        pmodels = list(~ comp_name,
                       ~ 1,
                       ~ comp_name)
      )

      plot(dose_effect, type = "all") 
    }
    
    
    
  })
  
  output$DoseResponseResiduals <- renderPlot({
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$delim,
    #   skip = input$skip
    # ) %>%
    #   prepare_data_shiny(
    #     df = ., 
    #     control = input$control,
    #     conc_as_categorical = input$conc_as_categorical
    #   ) -> data_clean
    # 
    
    test_compounds <- data_clean() 
    
    
    test_compounds <- test_compounds  %>%
      dplyr::filter(exp_type == "experiment")
    test_compounds <- test_compounds$comp_name %>% unique()
    
    negative_control <- data_clean() 
    
    negative_control <- negative_control %>%
      dplyr::filter(exp_type == input$control)
    
    negative_control <- negative_control$comp_name %>% unique()
    
    # if(length(test_compounds) == 1){
    
    #  data_clean <- data_clean
    #  } else {
    
    data_clean_drc <- data_clean() 
    
    data_clean_drc <- data_clean_drc %>%
      mutate(comp_name = ifelse(comp_name == negative_control,
                                test_compounds[1],
                                comp_name))
    # }
    
    if(input$control == "controlVehicleA"){
      
      data_clean_drc <- data_clean_drc %>%
        dplyr::filter(exp_type != "controlPositive",
                      exp_type != "controlNegative")
      
    } 
    
    if(input$control == "controlNegative"){
      
      data_clean_drc <- data_clean_drc %>%
        dplyr::filter(exp_type != "controlPositive")
      
    } 
    
    #model <- list(get(input$drc_model))
    
    
    if(length(test_compounds) == 1){
      
      dose_effect <- drc::drm(
        data = data_clean_drc,
        counts_norm ~ comp_concentration,
        #  curveid = comp_name,
        fct = LL.3()
      ) 
      
      plot(fitted(dose_effect),
           residuals(dose_effect),
           xlab = "Fitted values",
           ylab = "Raw residuals")
      
    } else {
      
      dose_effect <- drc::drm(
        data = data_clean_drc,
        counts_norm ~ comp_concentration,
        curveid = comp_name,
        fct = LL.3(),
        pmodels = list(~ comp_name,
                       ~ 1,
                       ~ comp_name)
      )
      
      plot(fitted(dose_effect),
           residuals(dose_effect),
           xlab = "Fitted values",
           ylab = "Raw residuals")
      
    }
    
    
    
  })

  
  
  output$Summary <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload an XLSX file"))
    
    readxl::read_excel(
      file$datapath,
      col_names = input$header,
      sheet = input$delim,
      skip = input$skip
    ) %>%
      prepare_data_shiny(
        df = ., 
        control = input$control,
        conc_as_categorical = input$conc_as_categorical) -> data_clean
    
    test_compounds <- data_clean %>%
      dplyr::filter(exp_type == "experiment")
    test_compounds <- test_compounds$comp_name %>% unique()
    
    negative_control <- data_clean %>%
      dplyr::filter(exp_type == input$control)
    negative_control <- negative_control$comp_name %>% unique()
    
    data_clean <- data_clean %>%
      mutate(comp_name = ifelse(comp_name == negative_control,
                                test_compounds[1],
                                comp_name))
    
    data_clean <- data_clean %>%
      dplyr::filter(exp_type != "controlPositive")
    
    f <- get(input$drc_model)
    
    if(length(test_compounds) > 1){
    dose_effect <- drc::drm(
      data = data_clean,
      counts_norm ~ comp_concentration,
      curveid = comp_name,
      fct = f(),
      pmodels = list(~ comp_name,
                     ~ 1,
                     ~ comp_name)
    )
    
    
    get_ec50_from_drc_model(drc_model_summary = dose_effect %>% summary())
    
    } else {
     
      
      
    dose_effect <- drc::drm(
        data = data_clean,
        counts_norm ~ comp_concentration,
        curveid = comp_name,
        fct = drc::LL.3())
    
    get_ec50_from_drc_model(drc_model_summary = dose_effect %>% summary())
      
      
      
    }
    # plot(dose_effect, type = "all")
    
  })
  
  ## rhandsontable
  df<- data.frame(c1=c(5,10,15), c2=c(3,6,9) , diff=c(0,0,0), select= as.logical( c(FALSE,FALSE,FALSE)))
  values <- reactiveValues(data = df)
  
  observe({
    if(!is.null(input$hot)){
      values$data <- as.data.frame(hot_to_r(input$hot))
      isolate(values$data[,'diff'] <- ifelse(values$data[,'select'], values$data[,'c1']-values$data[,'c2'] ,0))
      print(values$data)
      output$hot <- renderRHandsontable({
        rhandsontable(values$data)
      })
    }
  })    
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  
  ## Dunnett and boxplots
  output$Boxplot <- renderPlot({
   
    test_compounds <- data_clean() 
    
    test_compounds <- test_compounds %>%
      dplyr::filter(exp_type == "experiment")
    test_compounds <- test_compounds$comp_name %>% unique()
    
    negative_control <- data_clean() 
    
    
    negative_control <- negative_control %>%
      dplyr::filter(exp_type == input$control)
    negative_control <- negative_control$comp_name %>% unique()
    
    
    ## prepare data for Dunnett/Williams
    nested <- data_clean() 
    
    
    nested <- nested %>%
      group_by(exp_type, comp_name) %>%
      nest()
    
    #nested
    
    experiments <- nested %>%
      dplyr::filter(exp_type == "experiment")
    
    
    control_negative <- nested %>%
      dplyr::filter(exp_type == input$control)
    
    ## bind native control back to experiments
    bind_negative_control_back <- function(df1, df2) {
      dplyr::bind_rows(df1, df2)
    }
    
    
    experiments <- experiments %>%
      mutate(
        dunnett_data =
          map(
            .x = data,
            .f = bind_negative_control_back,
            df2 = control_negative$data
            
          )
      )
    
    #df = experiments$dunnett_data[[1]]
    #compound = experiments$comp_name[[1]]
    ## boxplots
    create_boxplots <- function(df, compound) {
      df %>%
        ggplot(aes(x = as_factor(comp_concentration), y = counts_norm)) +
        geom_boxplot() +
        geom_point(position = "jitter",
                   shape = 1,
                   colour = "blue") +
        stat_summary(fun = "mean", colour = "darkred") +
        ylim(c(
          min(df$counts_norm) - 0.1 * min(df$counts_norm),
          max(df$counts_norm) + 0.1 * max(df$counts_norm)
        ))  +
        xlab("Compound concentration") +
        ylab("Normalized counts") +
        ggtitle(compound) +
        theme_grey()
      
    }
    
    experiments <- experiments %>%
      mutate(boxplots =
               map2(.x = dunnett_data,
                    .y = comp_name,
                    .f = create_boxplots))
    
    cowplot::plot_grid(plotlist = experiments$boxplots,
                       ncol = nrow(experiments), scale = 0.9)
    
    
    # plot(dose_effect, type = "all")
    
  })
  
  
  ## dunnett
  output$Dunnettplot <- renderPlot({
    library(multcomp)
    
    # file <- input$file1
    # ext <- tools::file_ext(file$datapath)
    # 
    # req(file)
    # validate(need(ext == "xlsx", "Please upload an XLSX file"))
    # 
    # readxl::read_excel(
    #   file$datapath,
    #   col_names = input$header,
    #   sheet = input$delim,
    #   skip = input$skip
    # ) %>%
    #   prepare_data_shiny(
    #     df = ., 
    #     control = input$control,
    #     conc_as_categorical = input$conc_as_categorical
    #   ) -> data_clean
    
    
    test_compounds <- data_clean() 
    
    test_compounds <- test_compounds %>%
      dplyr::filter(exp_type == "experiment")
    test_compounds <- test_compounds$comp_name %>% 
      unique()
    
    negative_control <- data_clean() 
    
    negative_control <- negative_control %>%
      dplyr::filter(exp_type == input$control)
    negative_control <- negative_control$comp_name %>% 
      unique()
    
    
    ## prepare data for Dunnett/Williams
    nested <- reactive({data_clean() %>%
      group_by(exp_type, comp_name) %>%
      nest()})
    
    
    # nested
    
    
    experiments <- nested() 
    
    experiments <- experiments %>%
      dplyr::filter(exp_type == "experiment")
   # })
  #  control_negative <- nested() 
    
    control_negative <- reactive({
      nested() %>%
   #   control_negative %>%
      dplyr::filter(exp_type == input$control)
    })
      
    ## bind native control back to experiments
    bind_negative_control_back <- function(df1, df2){
      dplyr::bind_rows(df1, df2)
    }
    
    
   # experiments <- experiments() 
    
    experiments <- experiments %>%
      mutate(
        dunnett_data =
          map(
            .x = data,
            .f = bind_negative_control_back,
            df2 = control_negative()$data
            
          )
      )
    
    
    conf_plots <- reactive({
      
      map2(
      .x = experiments$dunnett_data,
      .y = experiments$comp_name,
      .f = perform_dunnett,
      variance_heterogeneity = input$variance_heterogeneity,
      alternative = input$side,
      type = "Dunnett"
    )
    
    })
    cowplot::plot_grid(plotlist = conf_plots(), 
                       ncol = nrow(experiments), scale = 0.9)
    
  })
  
  
}

#shinyApp(ui, server)
