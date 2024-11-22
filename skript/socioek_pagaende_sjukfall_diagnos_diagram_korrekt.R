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
  input_fil <- "pagaende_sjukfall_diagnos.xlsx"
  
  ohalsa_nr <- "32"
  
  flik_lista = list()
  
  if(uppdatera_data == TRUE){
    
    pagande_sjukfall_df = hamta_excel_dataset_med_url("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/sjp-pagaende-sjukfall-diagnos/SJPPagSjukfallDiagnosLan.xlsx",skippa_rader = 2) %>% 
      filter(År == max(År),
             Månad == first(Månad)) %>% 
        select(-kolumnnamn,-`Antal pågående sjukfall`) 
    
    
    pagande_sjukfall_df <- pagande_sjukfall_df %>% 
      mutate(Län = ifelse(Län == "Riket",paste0("00 ", Län),Län)) %>% 
        filter(Län%in% c("00 Riket","17 Värmlands län" ,"20 Dalarnas län","21 Gävleborgs län"),
               Diagnoskapitel %in% c("F00-F99 Psykiska sjukdomar och syndrom samt beteendestörningar","M00-M99 Sjukdomar i muskuloskeletala systemet och bindväven","Samtliga diagnoskapitel")) %>% 
          mutate(regionkod = substr(Län,1,2),
                 region = substr(Län,4,nchar(Län)),
                 månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B"))%>% 
            rename(Andel = `Andel pågående sjukfall (%)`) %>% 
              mutate(Andel = as.numeric(Andel))
    
    openxlsx::write.xlsx(pagande_sjukfall_df,paste0(input_mapp,input_fil))
  }else pagande_sjukfall_df <- rio::import(paste0(input_mapp,input_fil))
  
  #
  pagaende_sjukfall_grupperad <- pagande_sjukfall_df %>% 
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
