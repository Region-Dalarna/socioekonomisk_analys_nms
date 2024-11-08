skapa_UVAS_diagram <- function(spara_diagrambildfil = FALSE,
                               returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar diagram för innovationsindex uppdelat på län.
  # Verkar inte ha uppdaterats sedan 2021, varför jag använder samma källa för data som då. Bör uppdateras
  # så att data istället läggs i en mapp i projektet.
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  
  #input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
  input_mapp <- here("Indata/") %>% paste0(., "/")
  UVAS_fil <- "MUCF_20241108.xlsx"
  UVAS_full <- paste0(input_mapp, UVAS_fil)
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  ta_med_logga <- FALSE   # FALSE
  
  gg_list <- list()
  gg_list_map <- list()
  
  # ============================= diagram 14 ====================================
  UVAS_df <- read.xlsx(UVAS_full, sheet= 4) %>% 
    mutate(Andel = Andel * 100) 
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("UVAS_df", UVAS_df, envir = .GlobalEnv)
  }
  
  UVAS_df$Län <- factor(UVAS_df$Län, 
                          levels = c("Dalarna", "Gävleborg", "Värmland"))
  
  diagramfilnamn <- "UVAS_alder.png"
  
  
  # Uppdelat på åldersgrupp
  gg_obj <- SkapaStapelDiagram(skickad_df = UVAS_df %>% 
                                 filter(Bakgrund == "Totalt",
                                        Kön == "Totalt"),
                               skickad_x_var = "Län",
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "Ålder",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "procent",
                               x_axis_sort_value = FALSE,
                               manual_color = diagramfarger("gron_sex")[c(3,4)],
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = ta_med_logga)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  # Diagram för inrikes födda uppdelat på kön
  
  skapa_diagram <- function(df,bakgrund){
  
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   filter(Bakgrund == bakgrund,
                                          Kön != "Totalt"),
                                 skickad_x_var = "Ålder",
                                 skickad_y_var = "Andel",
                                 skickad_x_grupp = "Kön",
                                 output_mapp = mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_facet = TRUE,
                                 facet_grp = "Län",
                                 facet_scale = "fixed",
                                 facet_kolumner = 2,
                                 facet_legend_bottom = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "procent",
                                 x_axis_sort_value = FALSE,
                                 manual_color = diagramfarger("kon"),
                                 skriv_till_diagramfil = spara_diagrambildfil,
                                 lagg_pa_logga = ta_med_logga)
    
    gg_list_map <- c(gg_list_map, list(gg_obj))
  
  return(gg_list_map)}
  
  bakgrund <- c("Inrikes födda", "Utrikes födda")
  
  diag <- map(bakgrund, ~ skapa_diagram(UVAS_df, .x)) %>% flatten()
  # Diagram för utrikes födda uppdelat på kön
  
  gg_list <- c(gg_list, diag)
  
  names(gg_list) <- c("UVAS_alder", "UVAS_inrikes_födda", "UVAS_utrikes_födda")
  
  return(gg_list)
  
}
