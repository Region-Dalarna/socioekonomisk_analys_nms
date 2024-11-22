skapa_sjukpenningtal_lan <- function(uppdatera_data = TRUE,
                                     spara_diagrambildfil = FALSE,
                                     returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar två diagram för sjukpenningtal
  
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
  input_fil <- "sjukpenningtal.xlsx"

  flik_lista = list()
  
  if(uppdatera_data == TRUE){
  
    ohalsa_lista = hamta_excel_dataset_med_url("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-sjptal/SJPsjptal.xlsx",skippa_rader = 2) 
    
    # Eftersom dataset är så stort returneras det enbart i två listor. Vi behöver dataset i den första listan
  
    ohalsa_df <- ohalsa_lista[[1]] %>% 
      filter(År == max(År),
             Månad == first(Månad)) %>% 
        select(-kolumnnamn) 
          
    
    ohalsa_df <- ohalsa_df %>% 
      mutate(Län = ifelse(Län == "Riket",paste0("00 ", Län),Län),
             Kommun = ifelse(Kommun == "Riket",paste0("00 ", Kommun),Kommun)) %>% 
        filter(Kommun%in% c("00 Riket","17 Värmlands län" ,"20 Dalarnas län","21 Gävleborgs län")) %>% 
          mutate(regionkod = substr(Län,1,2),
                 region = substr(Län,4,nchar(Län)),
                 månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B")) %>% 
            rename(sjukpenningtal = `Sjukpenningtal 1.0`)
    
    openxlsx::write.xlsx(ohalsa_df,paste0(input_mapp,input_fil))
  }else ohalsa_df <- rio::import(paste0(input_mapp,input_fil))

  
  # Uppdelat på åldersgrupper (diagram 2 nedan)
  ohalsa_grupperad <- ohalsa_df %>% 
    group_by(regionkod,region,År,månad_namn,Ålder,Kön) %>%
      summarise(sjukpenningtal = sum(sjukpenningtal, na.rm = TRUE)) %>% 
        mutate(region = skapa_kortnamn_lan(region)) %>% 
          filter(Kön != "Kvinnor och män")
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sjukpenningtal_alder_df", ohalsa_grupperad, envir = .GlobalEnv)
  }
  
  # Totalt för 16-64 år (diagram 1 nedan)
  ohalsa_totalt <- ohalsa_grupperad %>% 
    filter(Ålder == "Samtliga 16-64 år")
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sjukpenningtal_totalt_df", ohalsa_totalt, envir = .GlobalEnv)
  }
  
  ohalsa_grupperad$region <- factor(ohalsa_grupperad$region, 
                                    levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 
  
  
  ohalsa_totalt$region <- factor(ohalsa_totalt$region, 
                                 levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 
  
  
  ohalsa_nr <- "30"
  
  diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_sjukpenningtal_kon_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ohalsa_totalt,
                               skickad_x_var = "region",
                               skickad_y_var = "sjukpenningtal",
                               skickad_x_grupp = "Kön",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = FALSE)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  ohalsa_nr <- "31"
  
  diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_sjukpenningtal_alder_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ohalsa_grupperad %>% 
                                 filter(Ålder != "Samtliga 16-64 år") %>% 
                                 separate(Ålder, into = c("Ålder", "Ålder2"), sep = " ", remove = FALSE),
                               skickad_x_var = "Ålder",
                               skickad_y_var = "sjukpenningtal",
                               skickad_x_grupp = "Kön",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               diagram_facet = TRUE,
                               facet_grp = "region",
                               facet_legend_bottom = TRUE,
                               facet_scale = "fixed",
                               manual_x_axis_title = "åldersgrupp",
                               manual_color = diagramfarger("kon"),
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = FALSE)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c("sjukpenningtal", "sjukpenningtal_alder")
  
  return(gg_list)
  
}
