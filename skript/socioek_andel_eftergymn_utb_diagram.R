




skapa_utb_eftergymn_kon_diagram <- function(region_vekt = c("00", "17", "20", "21"),
                                               valt_ar = NA,            # om NA tas det senaste tillgängliga året
                                               dia_titel = NULL, 
                                               #diagbild_bredd = 16,
                                               #diagbild_hojd = 9,
                                               spara_dataframe_till_global_environment = TRUE,
                                               spara_diagrambild = FALSE
){
    if (!require("pacman")) install.packages("pacman")
    p_load(here,
           tidyverse,
           pxweb)
    
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_utbniva_SCB.R")
  
    url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
    if (is.na(valt_ar)) uttag_ar <- as.character(hamta_giltiga_varden_fran_tabell(url_uttag, "tid")) %>% max() else uttag_ar <- as.character(valt_ar)
  
    px_df <- hamta_data_utbniva(region = region_vekt,
                                alder = c(as.character(25:64)),
                                tid = uttag_ar)
    
    utbniva <- px_df %>% 
      mutate(region = region %>% skapa_kortnamn_lan(),
             utbildningsnivå = ifelse(utbildningsnivå == "forskarutbildning", "eftergymnasial utbildning, 3 år eller mer", utbildningsnivå))
    
    # Av någon anledning så har SCB ändrat namnet på variabeln Befolkning till Antal. Jag byter tillbaka namn här för att slippa ändra längre ned i koden. /Jon 2025-01-08
    utbniva <- utbniva %>% 
      rename(Befolkning = Antal)
    
  # lägg ihop till Norra Mellansverige i egen df
  utbniva_nms <- utbniva %>% 
    filter(regionkod != "00") %>% 
    group_by(utbildningsnivå, kön, år) %>% 
    summarise(Befolkning = sum(Befolkning)) %>% 
    mutate(regionkod = "xxx",
           region = "Norra Mellansverige", 
           lansnamn = "Norra Mellansverige")
  
  # lägg till NMS till län och riket-df:n
  utbniva_tot <- utbniva %>% 
    bind_rows(utbniva_nms) %>%  
    group_by(år, regionkod, region, kön, utbildningsnivå) %>% 
    summarise(Befolkning = sum(Befolkning)) %>% 
    mutate(andel = Befolkning / sum(Befolkning), 
           utbildningsnivå = ifelse(utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer", "Eftergymn 3+ år", utbildningsnivå),
           utbildningsnivå = ifelse(utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år", "Eftergymn < 3 år", utbildningsnivå)) %>% 
    filter(str_detect(utbildningsnivå, "Eftergymn")) %>% 
    mutate(kön = tolower(kön),
           procent = round(andel * 100, 1),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige", "Riket"))) %>% 
    unite("grupp", c(utbildningsnivå, kön), remove = FALSE, sep = " ")
  
  if(spara_dataframe_till_global_environment) {
    assign("utb_niva_eftergymn_kon_df", utbniva_tot, envir = .GlobalEnv)
  }
  
  mapp <- here() %>% paste0(., "/")
  dia_filnamn <- "diagram_4_utbniva.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = utbniva_tot,
                    skickad_x_var = "region",
                    skickad_y_var = "procent",
                    skickad_x_grupp = "grupp",
                    diagram_titel = NULL,
                    filnamn_diagram = dia_filnamn,
                    diagram_capt = NULL,
                    output_mapp = mapp,
                    x_axis_lutning = 0,
                    stodlinjer_avrunda_fem = TRUE,
                    legend_rader = 2,
                    manual_y_axis_title = "procent",
                    #manual_x_axis_title = "år",
                    skriv_till_diagramfil = spara_diagrambild,
                    lagg_pa_logga = FALSE,
                    manual_color = diagramfarger("Kön_alla")[3:6])
  
  return(gg_obj)

}
