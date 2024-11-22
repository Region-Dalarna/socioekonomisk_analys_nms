skapa_ohalsotal_lan <- function(spara_diagrambildfil = FALSE,
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
  input_fil <- "ohalsotal.xlsx"
  
  if(uppdatera_data == TRUE){
    
    ohalsa_lista = hamta_excel_dataset_med_url("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-ohalsotal/SJPohttal.xlsx",skippa_rader = 2) 
    
    # Eftersom dataset är så stort returneras det enbart i två listor. Vi behöver dataset i den första listan
    
    ohalsa_df <- ohalsa_lista[[1]] %>% 
      filter(År == max(År),
             Månad == first(Månad)) %>% 
      select(-kolumnnamn) 
    
    
    ohalsa_df <- ohalsa_df %>% 
      mutate(Län = ifelse(Län == "Riket",paste0("00 ", Län),Län),
             Kommun = ifelse(Kommun == "Riket",paste0("00 ", Kommun),Kommun)) %>% 
        filter(Kommun%in% c("00 Riket","17 Värmlands län" ,"20 Dalarnas län","21 Gävleborgs län"),
               Ålder == "Samtliga 16-64 år") %>% 
          mutate(regionkod = substr(Län,1,2),
                 region = substr(Län,4,nchar(Län)),
                 månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B")) 
    
    openxlsx::write.xlsx(ohalsa_df,paste0(input_mapp,input_fil))
  }else ohalsa_df <- rio::import(paste0(input_mapp,input_fil))

  # Uppdelat på åldersgrupper 
  ohalsa_grupperad <- ohalsa_df %>% 
    group_by(regionkod,region,År,månad_namn,Ålder,Kön) %>%
      summarise(Ohalsotalet = sum(Ohälsotalet, na.rm = TRUE)) %>% 
        mutate(region = skapa_kortnamn_lan(region)) %>% 
          filter(Kön != "Kvinnor och män")
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("ohalsotal_df", ohalsa_grupperad, envir = .GlobalEnv)
  }
  
  ohalsa_grupperad$region <- factor(ohalsa_grupperad$region, 
                                    levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 
  
  ohalsa_nr <- "29"
  
  diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_ohalsotal_kon_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ohalsa_grupperad,
                               skickad_x_var = "region",
                               skickad_y_var = "Ohalsotalet",
                               skickad_x_grupp = "Kön",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = FALSE)
  
  return(gg_obj)
  
}
