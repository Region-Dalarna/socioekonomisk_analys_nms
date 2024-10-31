

skapa_invandring_diagram <- function(
    region_vekt = c("17", "20", "21"),
    valt_ar = NA,
    dia_titel = NULL,
    spara_diagrambildfil = FALSE
    ){

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/BefforandrKvRLK"
  if (all(is.na(valt_ar))) uttag_ar <- as.character(max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))) else uttag_ar <- as.character(valt_ar)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_period_kon_scb.R")
  
    px_df <- suppress_specific_warning(
      hamta_bef_forandringar_region_alder_kon_scb(region_vekt = region_vekt,
                                                  forandringar_klartext = "invandringar",
                                                  period_klartext = "hel",
                                                  kon_klartext = NA,
                                                  tid_koder = c("2000":uttag_ar)),
      "argument is not numeric or logical: returning NA")
    
      
  utr_inflyttning <- px_df %>% 
    filter(!is.na(personer)) %>% 
    mutate(region = region %>% skapa_kortnamn_lan())
  
  dia_filnamn <- "diagram_3_utr_inflyttning.png"
  mapp <- here("figurer/") %>% paste0(., "/")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = utr_inflyttning,
                    skickad_x_var = "år",
                    skickad_y_var = "personer",
                    skickad_x_grupp = "region",
                    diagram_titel = dia_titel,
                    filnamn_diagram = dia_filnamn,
                    diagram_capt = NULL,
                    output_mapp = mapp,
                    #y_axis_borjar_pa_noll = FALSE,
                    manual_y_axis_title = "antal inflyttare från utlandet",
                    #manual_x_axis_title = "år",
                    lagg_pa_logga = FALSE,
                    manual_color = diagramfarger("bla_gra_tre"),
                    skriv_till_diagramfil = spara_diagrambildfil)
  
  return(gg_obj)

} # slut funktion
