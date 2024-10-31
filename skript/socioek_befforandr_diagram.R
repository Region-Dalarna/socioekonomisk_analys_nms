if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       pxweb)

skapa_beffor_diagram <- function(region_vekt = c("17", "20", "21"),
            valt_ar = "9999",                 # "9999" = senaste år
            dia_titel = NULL,
            spara_diagrambildfil = FALSE,
            returnera_dataframe_global_environment = TRUE
            ){

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_period_kon_scb.R")

  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/BefforandrKvRLK"

  giltiga_ar <- hamta_giltiga_varden_fran_tabell(url_uttag, "tid")
  uttag_ar <- if (all(valt_ar != "*")) valt_ar %>% as.character() %>% str_replace("9999", max(giltiga_ar)) %>% .[. %in% giltiga_ar] %>% unique() else giltiga_ar

  repeat {
    px_df <- suppress_specific_warning(
      hamta_bef_forandringar_region_alder_kon_scb(region_vekt = region_vekt,
                                                forandringar_klartext = c("födelseöverskott", "flyttningsöverskott övriga Sverige", "invandringsöverskott"),
                                                period_klartext = "hel",
                                                kon_klartext = c("män", "kvinnor"),
                                                tid_koder = uttag_ar),
    "argument is not numeric or logical: returning NA")
    
    if (all(is.na(px_df$personer))) {
      uttag_ar <- as.character(as.numeric(uttag_ar) -1)
    } else {
      break
    }
  
  }
  
  beffor <- px_df %>% 
    mutate(region = region %>% skapa_kortnamn_lan(),
           förändringar = case_when (förändringar == "födelseöverskott" ~ "Födelsenetto",
                                     förändringar == "flyttningsöverskott övriga Sverige" ~ "Inrikes flyttnetto",
                                     förändringar == "invandringsöverskott" ~ "Utrikes flyttnetto")) %>% 
    unite("grupp", c(förändringar, kön), remove = FALSE, sep = " ") %>% 
    mutate(grupp = factor(grupp, levels = c("Utrikes flyttnetto män", "Utrikes flyttnetto kvinnor",
                                                  "Inrikes flyttnetto män", "Inrikes flyttnetto kvinnor",
                                                  "Födelsenetto män", "Födelsenetto kvinnor")))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("beffor_df", beffor, envir = .GlobalEnv)
  }
  
  dia_filnamn <- "diagram_2_befforandringar.png"
  mapp <- here("figurer/") %>% paste0(., "/")
  
  gg_obj <- suppress_specific_warning(
    SkapaStapelDiagram(skickad_df = beffor,
                    skickad_x_var = "grupp",
                    skickad_y_var = "personer",
                    skickad_x_grupp = "region",
                    diagram_titel = dia_titel,
                    filnamn_diagram = dia_filnamn,
                    diagram_capt = NULL,
                    output_mapp = mapp,
                    #y_axis_borjar_pa_noll = FALSE,
                    manual_y_axis_title = "förändring, antal invånare",
                    #manual_x_axis_title = "år",
                    lagg_pa_logga = FALSE,
                    diagram_liggande = TRUE,
                    x_axis_lutning = 0,
                    manual_color = diagramfarger("bla_gra_tre"),
                    skriv_till_diagramfil = spara_diagrambildfil
                    ),"argument is not numeric or logical: returning NA")
  
  return(gg_obj)

}