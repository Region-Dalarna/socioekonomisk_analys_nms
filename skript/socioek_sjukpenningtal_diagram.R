skapa_sjukpenningtal_lan <- function(spara_diagrambildfil = FALSE,
                                    returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar två diagram för sjukpenningtal
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  # För källa till data, se viktig info under Indata
  input_mapp <- here("Indata/") %>% paste0(., "/")
  input_fil <- "sjukpenningtal_2024_11_06.xlsx"
  
  ohalsa_nr <- "30"

  ohalsa_df <- rio::import(paste0(input_mapp,input_fil)) %>% 
    mutate(Län = ifelse(Län == "Riket",paste0("00 ", Län),Län),
           Kommun = ifelse(Kommun == "Riket",paste0("00 ", Kommun),Kommun)) %>% 
    filter(Kommun%in% c("00 Riket","17 Värmlands län" ,"20 Dalarnas län","21 Gävleborgs län")) %>% 
    mutate(regionkod = substr(Län,1,2),
           region = substr(Län,4,nchar(Län))) %>% 
    rename(sjukpenningtal = `Sjukpenningtal 1.0`) %>% 
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
                               skriv_till_diagramfil = spara_diagrambildfil)
            
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
                             skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c("sjukpenningtal", "sjukpenningtal_alder")
  
  return(gg_list)
  
}
