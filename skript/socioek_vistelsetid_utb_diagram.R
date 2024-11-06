skapa_vistelsetid_utbildning_lan <- function(region_vekt = c("17", "20", "21"),
                                            spara_diagrambildfil = FALSE,
                                            returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar diagram för vistelsetid uppdelat på utbildningsnivå och bakgrundsvariabel (facet på län)
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1LanKonUtb_scb.R")
  
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  px_df <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb(region_vekt = region_vekt,
                                                                      kon_klartext = "män och kvinnor",
                                                                      utbniv_klartext = "*",
                                                                      bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                      cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
                                                                      tid_koder = "9999") %>% 
    filter(!(utbildningsnivå %in% c("samtliga utbildningsnivåer","utbildningsnivå: uppgift saknas"))) %>% 
      separate(utbildningsnivå, into = c("bort", "utbildningsnivå","bort_2"), sep = " ", remove = FALSE) %>% 
        select(-bort,-bort_2) %>% 
          mutate(utbildningsnivå = paste0(toupper(substr(utbildningsnivå,1,1)),substr(utbildningsnivå,2,nchar(utbildningsnivå))),
                 region = skapa_kortnamn_lan(region)) %>% 
            rename(andel_forvarvsarbetande = `Andel förvärvsarbetande (ny definition från och med 2019)`)
    
  if(returnera_dataframe_global_environment == TRUE){
    assign("vistelsetid_utb_df", px_df, envir = .GlobalEnv)
  }
  
  # Make factor variable of utbildningsnivå
  px_df$utbildningsnivå <- factor(px_df$utbildningsnivå, levels = unique(px_df$utbildningsnivå))
  
  # Make factor variable of bakgrundsvariabel
  px_df$bakgrundsvariabel <- factor(px_df$bakgrundsvariabel, levels = unique(px_df$bakgrundsvariabel))
  
  diagramfilnamn <- "diagram_24_vistelsetid_utbniva.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = px_df,
                               skickad_x_var = "utbildningsnivå",
                               skickad_y_var = "andel_forvarvsarbetande",
                               skickad_x_grupp = "bakgrundsvariabel",
                               x_axis_sort_value = FALSE,
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_y_axis_title = "procent",
                               x_axis_lutning = 0,
                               y_axis_100proc = TRUE,
                               diagram_facet = TRUE,
                               facet_kolumner = 2,
                               facet_grp = "region",
                               facet_scale = "fixed",
                               facet_legend_bottom = TRUE,
                               manual_color =rev(diagramfarger("gron_sex")),
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  
  return(gg_obj)
  
}
