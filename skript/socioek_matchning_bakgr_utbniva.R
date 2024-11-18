
skapa_matcning_utbniva_bakgrund_diagram <- function(
    logga_path = NA,
    logga_i_diagram = FALSE,
    ta_bort_diagram_titel = FALSE,
    skriv_diagramfil = FALSE,
    diag_fargvekt = NA,
    output_mapp = NA,
    returnera_dataframe_global_environment = TRUE
) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # hantering av output mapp
  if (all(is.na(output_mapp)) & skriv_diagramfil) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde om du vill skriva en diagrambildfil.")
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("bla_gra_fyra")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }

  gg_list <- list()
  
  data_fil <- here("Indata","utbniva_matchning.xlsx")
  
  matchning_df <- read_xlsx(data_fil) %>% 
    mutate(UtbNiva_match_txt = factor(UtbNiva_match_txt, 
                                      levels = c("För låg utbildningsnivå",
                                               "Rätt utbildningsnivå",
                                               "För hög utbildningsnivå")),
           Ar = as.character(Ar)) 
  
  diagramfilnamn <- "matchning_utbniva_bakgr.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = matchning_df,
                               skickad_x_var = "Ar",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "inr_utr_fodd",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               stodlinjer_avrunda_fem = TRUE,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_lutning = 45,
                               geom_position_stack = FALSE,
                               manual_y_axis_title = "procent",
                               manual_color = diag_fargvekt,
                               skriv_till_diagramfil = skriv_diagramfil,
                               utan_diagramtitel = ta_bort_diagram_titel,
                               diagram_facet = TRUE,
                               facet_grp = "UtbNiva_match_txt",
                               facet_scale = "fixed",
                               facet_legend_bottom = TRUE,
                               #legend_vand_ordning = TRUE,
                               lagg_pa_logga = logga_i_diagram,
                               logga_path = logga_path
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

  return(gg_list)
}
