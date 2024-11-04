

skapa_ohalsotal_diagram <- function(region_vekt = c("00","17", "20", "21"),
                                     dia_titel = NULL,
                                     spara_diagrambildfil = FALSE,
                                     returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar ohälsotalet uppdelat på kön för ett antal regioner. Enbart senaste år
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         pxweb,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  kolada_id <- "N00957"
  
  ohalsa_df <- hamta_kolada_df(kpi_id = kolada_id,
                               valda_kommuner = region_vekt,
                               valda_ar = valt_ar,
                               konsuppdelat = TRUE,
                               dop_om_kolumner = TRUE) %>% 
    mutate(region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")))

  
  if(returnera_dataframe_global_environment == TRUE){
    assign("ohalsa_df", ohalsa_df, envir = .GlobalEnv)
  }
  
  ohalsa_nr <- "29"
  
  diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_ohalsotal_kon_NMS_riket.png")
  mapp <- here("figurer/") %>% paste0(., "/")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ohalsa_df,
                               skickad_x_var = "region",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "kon",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               stodlinjer_avrunda_fem = TRUE,
                               manual_x_axis_title = dia_titel,
                               manual_y_axis_title = "Ohälsotal",
                               manual_color = diagramfarger("kon"),
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = FALSE)
  
  return(gg_obj)
  
}