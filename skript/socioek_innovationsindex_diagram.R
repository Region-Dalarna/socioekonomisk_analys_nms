skapa_innovationsindex_diagram <- function(region_vekt = c("00","17", "20", "21"),
                                          ta_bort_titel = TRUE,
                                          ta_bort_caption = TRUE,
                                          spara_diagrambildfil = FALSE,
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

  
  input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
  innovationsindex_fil <- "innovationsindex.csv"
  innovationsindex_full <- paste0(input_mapp, innovationsindex_fil)
  
  diagram_capt <- "Källa: Reglab\nBearbetning: Samhällsanalys, Region Dalarna"
  ta_med_logga <- FALSE   # FALSE
  
  # ============================= diagram 14 ====================================
  innind <- read.csv(innovationsindex_full, encoding = "UTF-8")
  
  names(innind)[1] <- "År"
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("innovationsindex_df", innind, envir = .GlobalEnv)
  }
  
  innind <- innind %>% 
    mutate(fokus = ifelse(Region %in% c("Dalarna", "Gävleborg", "Värmland"), 1,0))
  
  diag_nr <- "46"
  diagramfilnamn <- paste0("diagram_",diag_nr,"innovationsindex.png")
  diagramtitel <- paste0("Reglabs innovationsindex år ", max(innind$År)) 
  
  if(ta_bort_titel) diagramtitel <- NULL
  if(ta_bort_caption) diagram_capt <- NULL
  
  gg_obj <- SkapaStapelDiagram(skickad_df = innind %>% filter(År == max(År)),
                               skickad_x_var = "Region",
                               skickad_y_var = "Index",
                               skickad_x_grupp = NA,
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "innovationsindex",
                               x_axis_sort_value = TRUE,
                               x_var_fokus = "fokus",
                               diagram_capt = diagram_capt,
                               diagram_titel = diagramtitel,
                               manual_color = diagramfarger("gron_tva_fokus_morkgron"),
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = ta_med_logga)
}
