## load helpers
source(
  here::here(
    "data-raw",
    "DATASETS.R"
  )
)
##################### FUNCTIONS 
library(rhandsontable)
#library(survival)
#library(codetools)
library(tidyverse)
library(readxl)
#library(pastecs)
#library(emmeans)
#library(drc)
#library(lmtest)
#library(sandwich)
#library(ordinal)
#library(rstatix)
#library(ez)
library(mratios)
library(multcomp)
library(drc)

normalize_for_negative_control <- function(df, control){
  
  mean_vehicle <- df %>%
    dplyr::filter(exp_type == control) %>%
    dplyr::select(raw_data) %>%
    purrr::flatten() %>%
    as.integer() %>%
    mean(na.rm = TRUE)
  
  df <- df %>%
    dplyr::mutate(counts_norm = raw_data/mean_vehicle)
  
  return(df)
}


set_vehicle_to_zero <- function(df, 
                                conc_as_categorical = FALSE, 
                                control){
  
  if(conc_as_categorical == TRUE){
    zero = "0"
  } else {
    zero = 0
  }
  
  ind <- df$exp_type == control 
  df[ind, "comp_concentration"] <- zero
    return(df)
}



prepare_data_shiny <- function(df, conc_as_categorical, ...){
  
  data_clean <- df %>%
    dplyr::select(
      vialNr,
      dropCode,
      expType,
      RawData,
      compName,
      compConcentration,
      compVehicle) %>%
    janitor::clean_names() %>%
    na.omit %>%
    normalize_for_negative_control(
      df = ., 
      ...) %>% # generate input for control from selectInput
    set_vehicle_to_zero(
      df = .,
      conc_as_categorical = conc_as_categorical,
      ...) %>%
    group_by(vial_nr, 
             comp_name, 
             comp_concentration,
             exp_type) %>%
    summarise(counts_norm = mean(counts_norm)) 
  
  data_clean
  
}


# get tidy model params
get_ec50_from_drc_model <- function(drc_model_summary){
  
  
  df <- drc_model_summary$coefficients %>%
    as.data.frame()
  params <- row.names(df) 
  df$params <- params
  
  df <- df %>%
    tidyr::separate(
      col = params, 
      into = c("model_params", "chemical"), 
      sep = ":") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(chemical, model_params, estimate, std_error, t_value, p_value) %>%
    dplyr::mutate(model_text = drc_model_summary$text,
                  distribution_assumption = drc_model_summary$type)
  
  return(df)
  
}


## test df 

df = tibble(
  comp_concentration = c(0,0,0,0,
                         2,2,2,2,
                         8,8,8,8,
                         16,16,16,16),
  counts_norm = c(1.3,0.7,1.2,0.8,
                  5.6,7.6,4,5.1,
                  10.4, 11.2, 31.56,14.5,
                  40.9, 50.3, 45.6, 38.9)
)


perform_dunnett <- function(
  df = NULL, 
  compound = NULL, 
  variance_heterogeneity = TRUE,
  alternative = "less",
  type = "Dunnett"){
  
  if(variance_heterogeneity == FALSE){
  
  df <- df %>%
    dplyr::mutate(comp_concentration = as_factor(comp_concentration))
  
  model <- lm(counts_norm ~ comp_concentration, data = df)

  model <- multcomp::glht(
    model, linfct = mcp(
      comp_concentration = type
    ), alternative = alternative
  ) 

  model %>% 
    confint() %>%
    broom::tidy() %>%
    janitor::clean_names() %>%
    mutate(rowid = 1:nrow(.)) %>%
    ggplot(aes(x = estimate, y = reorder(as_factor(contrast), -rowid))) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(xmin=estimate-(estimate-conf_low), 
          xmax=estimate+(conf_high-estimate), 
          width=.2),
          size = 1
      ) +
    geom_vline(xintercept = 0, 
               linetype = "dashed", 
               colour = "darkred",
               size = 1) +
    xlab("Difference to control") +
    ylab("Contrast") +
    ggtitle(paste(
      "95% conf. int \n", 
      compound
      )) -> plot
  

  } 
  
  if(variance_heterogeneity == TRUE){
  
    
    model <- mratios::sci.ratioVH(
             counts_norm ~ as_factor(comp_concentration), 
             data = df, 
             type = "Dunnett",
             alternative = alternative
             )
    
    ## create dataframe:
    model$estimate %>% 
      as.data.frame() %>%
      janitor::clean_names() %>%
      dplyr::rename(estimate = v1) %>%
      mutate(term = "comp_concentration",
              contrast = rownames(.)) %>%
      as_tibble -> estimates
    
    
    if(alternative == "less"){
    
  model$conf.int %>%
      dplyr::rename(
        conf_high = upper
      ) -> confintervals
    }
    
    if(alternative == "greater"){
      
      model$conf.int %>%
        dplyr::rename(
          conf_low = lower
        ) -> confintervals
    }
    
    if(alternative == "two.sided"){
      
      model$conf.int %>%
        dplyr::rename(
          conf_low = lower,
          conf_high = upper
        ) -> confintervals
    }
    
   model_df <- dplyr::bind_cols(
      estimates,
      confintervals
   ) %>%
      mutate(rowid = 1:nrow(.)) 
    
   
   
   errors <- if(alternative == "less"){
     geom_errorbar(
       aes(xmin=estimate-Inf, 
           xmax=estimate+(conf_high-estimate), 
           width=.2),
       size = 1)} else if(alternative == "two.sided"){
         geom_errorbar(
           aes(xmin=estimate-(estimate-conf_low), 
               xmax=estimate+(conf_high-estimate), 
               width=.2),
           size = 1)} else if(alternative == "greater"){
             geom_errorbar(
               aes(xmin=estimate-(estimate-conf_low), 
                   xmax=estimate+Inf, 
                   width=.2),
               size = 1)}
   
   
   
   model_df %>% 
      ggplot(aes(x = estimate, y = reorder(as_factor(contrast), -rowid))) +
      geom_point(size = 3) +
      errors +
      geom_vline(
        xintercept = 1, 
        linetype = "dashed", 
        colour = "darkred") +
      xlab("Ratio-to-control") +
      ylab("Contrast") +
      ggtitle(paste(
        "95% conf. int, \n", 
        compound
      )) -> plot
    
  
    
  }
  
  return(plot)
  
}



  
