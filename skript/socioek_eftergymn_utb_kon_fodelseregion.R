skapa_utbniva_bakgrund_kon_diagram <- function(
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_integration_region_kon_bakgrund_tid_IntGr3LanKONS_IntGr3RikKONS_scb.R")
  # ========================================== Inställningar ============================================
  

  px_alla <- hamta_integration_region_bakgrund_tid_kon_scb(region_vekt = region_vekt,
                                                           kon_klartext = c("kvinnor", "män"),
                                                           bakgrund_klartext = "andel med eftergymnasial utbildning, procent",
                                                           cont_klartext = c("Födda i Sverige", "Födda i Norden exkl. Sverige", 
                                                                                 "Födda i EU/EFTA exkl. Norden", "Födda i övriga världen"),
                                                           tid_koder = valt_ar)
  
  df_dia <- px_alla %>%
    mutate(region = region %>% skapa_kortnamn_lan(FALSE),
           kön = tolower(kön),
           bakgrund = str_remove(bakgrund, "Födda i "),
           fodelseregion_radbryt = dela_upp_strang_radbryt(bakgrund, max_langd = 10),
           fodelseregion_radbryt = ifelse(fodelseregion_radbryt == "övriga\nvärlden", "Övriga\nvärlden", fodelseregion_radbryt),
           fodelseregion_radbryt = factor(df_dia$fodelseregion_radbryt, 
                                           levels = unique(df_dia$fodelseregion_radbryt)),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")))
  
  if(spara_dataframe_till_global_environment) {
    assign("utbniva_eftergymn_bakgr_kon_df", df_dia, envir = .GlobalEnv)
  }
  
  
  if (is.na(output_mapp)) output_mapp <- here() %>% paste0(., "/")
  diagramfilnamn <- "diagram_7_eftergymnutb_kon_fodelseregion.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df_dia,
                     skickad_x_var = "fodelseregion_radbryt",
                     skickad_y_var = "varde",
                     skickad_x_grupp = "kön",
                     output_mapp = output_mapp,
                     filnamn_diagram = diagramfilnamn,
                     #manual_x_axis_text_vjust = 1,
                     #manual_x_axis_text_hjust = 1,
                     manual_y_axis_title = "procent",
                     facet_legend_bottom = TRUE,
                     x_axis_lutning = 0,
                     diagram_facet = TRUE,
                     facet_x_axis_storlek = 10.5,
                     facet_y_axis_storlek = 12,
                     facet_scale = "fixed",
                     facet_grp = "region",
                     procent_0_100_10intervaller = TRUE,
                     manual_color = diagramfarger("kon"),
                     skriv_till_diagramfil = spara_diagrambildfil,
                     lagg_pa_logga = FALSE)
  
  return(gg_obj)
}
