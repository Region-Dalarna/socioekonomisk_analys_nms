skapa_gymn_behorighet_inr_utr_fodda_kon_diagram <- function(
    region_vekt = c( "20", "17", "21", "00"),
    valt_ar = "9999",                  # "9999" = senaste år
    dia_titel = NULL,
    output_mapp = NA,
    spara_dataframe_till_global_environment = TRUE,
    spara_diagrambildfil = FALSE
){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_IntGr8RikKON2_IntGr8LanKON2_scb.R")
  # ========================================== Inställningar ============================================
  

  px_df <- hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_scb(
    region_vekt = region_vekt,
    kon_klartext = c("kvinnor", "män"),
    bakgrund_klartext = c("födelseregion: Sverige", "samtliga utrikes födda invandrare"),
    cont_klartext = "Andel behöriga till gymnasium, procent",
    tid_koder = valt_ar)
  
  diag_df <- px_df %>% 
    distinct() %>% 
    mutate(region = region %>% skapa_kortnamn_lan(FALSE),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")),
           variabel = ifelse(variabel == "födelseregion: Sverige", "Inrikes födda", "Utrikes födda")) %>% 
    rename(andel = `Andel behöriga till gymnasium, procent`) %>% 
    unite("grupp", c(variabel, kön), remove = FALSE, sep = " ")
  
  if(spara_dataframe_till_global_environment) {
    assign("gymn_behorighet_bakgr_kon_df", diag_df, envir = .GlobalEnv)
  }
  
  if (is.na(output_mapp)) output_mapp <- here() %>% paste0(., "/")
  diagramfilnamn <- "gymn_behorighet_bakgr_kon.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = diag_df,
                     skickad_x_var = "region",
                     skickad_y_var = "andel",
                     skickad_x_grupp = "grupp",
                     output_mapp = output_mapp,
                     filnamn_diagram = diagramfilnamn,
                     #manual_x_axis_text_vjust = 1,
                     #manual_x_axis_text_hjust = 1,
                     manual_y_axis_title = "procent",
                     facet_legend_bottom = TRUE,
                     x_axis_lutning = 0,
                     # diagram_facet = FALSE,
                     # facet_x_axis_storlek = 10.5,
                     # facet_y_axis_storlek = 12,
                     # facet_scale = "fixed",
                     # facet_grp = "region",
                     procent_0_100_10intervaller = TRUE,
                     manual_color = diagramfarger("Kön_alla")[3:6],
                     skriv_till_diagramfil = spara_diagrambildfil,
                     lagg_pa_logga = FALSE)
  
  return(gg_obj)
}
