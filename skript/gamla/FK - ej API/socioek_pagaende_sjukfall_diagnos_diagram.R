skapa_pagaende_diagnos_lan <- function(spara_diagrambildfil = FALSE,
                                returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar ett diagram för ohälsotal
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl,
         rio)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  # För källa till data, se viktig info under Indata
  input_mapp <- here("Indata/") %>% paste0(., "/")
  input_fil <- "pagaende_sjukfall_diagnos_2024_11_06.xlsx"
  
  ohalsa_nr <- "32"
  
  pagande_sjukfall_df <- rio::import(paste0(input_mapp,input_fil)) %>% 
    mutate(Län = ifelse(Län == "Riket",paste0("00 ", Län),Län)) %>% 
    filter(Län%in% c("00 Riket","17 Värmlands län" ,"20 Dalarnas län","21 Gävleborgs län")) %>% 
    mutate(regionkod = substr(Län,1,2),
           region = substr(Län,4,nchar(Län))) %>%
    filter(År == max(År)) %>% 
    filter(Månad == max(Månad)) %>% 
    mutate(månad_namn = case_when(Månad == "01" ~ "januari",
                                  Månad == "02" ~ "februari",
                                  Månad == "03" ~ "mars",
                                  Månad == "04" ~ "april",
                                  Månad == "05" ~ "maj",
                                  Månad == "06" ~ "juni",
                                  Månad == "07" ~ "juli",
                                  Månad == "08" ~ "augusti",
                                  Månad == "09" ~ "september",
                                  Månad == "10" ~ "oktober",
                                  Månad == "11" ~ "november",
                                  Månad == "12" ~ "december"))
  
  #
  pagaende_sjukfall_grupperad <- pagande_sjukfall_df %>% 
    group_by(regionkod,region,År,månad_namn,Kön,Diagnoskapitel) %>%
      summarise(Andel = sum(`Andel pågående sjukfall (%)`, na.rm = TRUE)) %>% 
        mutate(region = skapa_kortnamn_lan(region)) %>% 
          filter(Kön != "Kvinnor och män") %>% 
            mutate(Diagnoskapitel = case_when(Diagnoskapitel == "F00-F99 Psykiska sjukdomar och syndrom samt beteendestörningar" ~ "Psykisk ohälsa",
                                              Diagnoskapitel == "M00-M99 Sjukdomar i muskuloskeletala systemet och bindväven" ~ "Muskoskeletära diagnoser",
                                              Diagnoskapitel == "Samtliga diagnoskapitel" ~ "Samtliga"))
                                              
  
  # Pivot wider based on diagnoskapitel
  pagaende_sjukfall_grupperad <- pagaende_sjukfall_grupperad %>% 
    pivot_wider(names_from = Diagnoskapitel, values_from = Andel) %>% 
      mutate(Övriga = Samtliga - `Psykisk ohälsa` - `Muskoskeletära diagnoser`) %>% 
        pivot_longer(cols = c(`Psykisk ohälsa`, `Muskoskeletära diagnoser`, "Övriga"),
                     names_to = "Diagnoskapitel",
                     values_to = "Andel") %>% 
          select(-Samtliga)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sjukfall_diagnos_df", pagaende_sjukfall_grupperad, envir = .GlobalEnv)
  }
  
  pagaende_sjukfall_grupperad$region <- factor(pagaende_sjukfall_grupperad$region, 
                                    levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 
  
  
  diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_ohalsotal_kon_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = pagaende_sjukfall_grupperad,
                               skickad_x_var = "Kön",
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "Diagnoskapitel",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               diagram_facet = TRUE,
                               facet_grp = "region",
                               facet_legend_bottom = TRUE,
                               facet_scale = "free",
                               manual_x_axis_title = "kön",
                               manual_y_axis_title = "procent",
                               #manual_color = diagramfarger("kon"),
                               y_axis_100proc = TRUE,
                               geom_position_stack = TRUE,
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = FALSE)
  
  return(gg_obj)
  
}
