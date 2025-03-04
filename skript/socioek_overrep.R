skapa_overrep_diagram <- function(spara_diagrambildfil = FALSE,
                                 diag_fargvekt = NA, # För diagrammet som inte är könsuppdelat
                                 returnera_dataframe_global_environment = TRUE,
                                 ta_bort_titel = TRUE,
                                 ta_bort_caption = TRUE,
                                 ta_bort_y_axis_title = TRUE,
                                 diagramtitel = "Överrepresentation av manliga chefer",
                                 input_mapp = NA, # Vid NA, väljs mappen Indata i det projekt man arbetar med
                                 mapp = NA # Som ovan fast figurer
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
  if(is.na(input_mapp)){
    input_mapp <- here("Indata/") %>% paste0(., "/")} else{
      input_mapp <- input_mapp
    }
  # overrep_fil <- "overrep.xlsx"
  # overrep_full <- paste0(input_mapp, overrep_fil)
  
  files <- list.files(input_mapp, pattern = "*overrep", full.names = TRUE)
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  if(is.na(input_mapp)){
    mapp <- here("figurer/") %>% paste0(., "/")} else{
      mapp <- mapp
    }
  
  #mapp <- here("figurer/") %>% paste0(., "/")
  
  ta_med_logga <- FALSE   # FALSE
  
  gg_list <- list()
  
  # ============================= diagram 14 ====================================
  overrep_df <- read.xlsx(latest_file) %>% 
    filter(SNI2007_Grupp_namn != "Okänt")
  
  
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("overrep_df", overrep_df, envir = .GlobalEnv)
  }
  
  # UVAS_df$Län <- factor(UVAS_df$Län, 
  #                       levels = c("Dalarna", "Gävleborg", "Värmland"))
  
  diagramfilnamn <- "overrep.png"

  diagram_capt <- "Källa: SCB, egna bearbetningar.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Över-/underrepresentation för andel män i ledningsyrken i relation till andelen män inom branschen.\nEtt värde på 0 innebär att andelen manliga chefer är proportionerlig gentemot andelen manliga anställda.\nEtt värde över 0 innebär att män är överrepresenterade i ledande positioner medan ett värde under 0 innebär en underrepresentation."
  y_axis_title = "Överrepresentation av manliga chefer"
  
  if(ta_bort_titel) diagramtitel <- NULL
  if(ta_bort_caption) diagram_capt <- NULL
  if(ta_bort_y_axis_title) y_axis_title <- NULL
  
  
  # Uppdelat på overrep_df
  gg_obj <- SkapaStapelDiagram(skickad_df = overrep_df,
                               skickad_x_var = "SNI2007_Grupp_namn",
                               skickad_y_var = "overrep",
                               skickad_x_grupp = "Ar",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = y_axis_title,
                               x_axis_sort_value = FALSE,
                               diagram_capt = diagram_capt,
                               diagram_titel = diagramtitel,
                               manual_color = diag_fargvekt,
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = ta_med_logga)
  
  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- c("overrep")
  
  return(gg_list)
  
}
