skapa_overrep_diagram <- function(spara_diagrambildfil = FALSE,
                                 diag_fargvekt = NA, # För diagrammet som inte är könsuppdelat
                                 returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar överrepresentation av chefer. Data har hämtats av Kristoffer Sehlberg (region Gävleborg) i SCB/MONA.
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("bla_gra_fyra")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  
  #input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
  input_mapp <- here("Indata/") %>% paste0(., "/")
  overrep_fil <- "overrep.xlsx"
  overrep_full <- paste0(input_mapp, overrep_fil)
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  ta_med_logga <- FALSE   # FALSE
  
  gg_list <- list()
  
  # ============================= diagram 14 ====================================
  overrep_df <- read.xlsx(overrep_full) 
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("overrep_df", overrep_df, envir = .GlobalEnv)
  }
  
  # UVAS_df$Län <- factor(UVAS_df$Län, 
  #                       levels = c("Dalarna", "Gävleborg", "Värmland"))
  
  diagramfilnamn <- "overrep.png"
  
  
  # Uppdelat på overrep_df
  gg_obj <- SkapaStapelDiagram(skickad_df = overrep_df,
                               skickad_x_var = "SNI2007_Grupp_namn",
                               skickad_y_var = "overrep",
                               skickad_x_grupp = "Ar",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "Överrepresentation av manliga chefer",
                               x_axis_sort_value = FALSE,
                               manual_color = diag_fargvekt,
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = ta_med_logga)
  
  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- c("overrep")
  
  return(gg_list)
  
}
