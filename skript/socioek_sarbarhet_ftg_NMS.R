skapa_sarbarhet_ftg_diagram <- function(spara_diagrambildfil = FALSE,
                                        diag_fargvekt = NA, # För diagrammet som inte är könsuppdelat
                                        returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar ett diagram över beroende av företag i NMS. Data från NMS, Ftg_50procent_lonesumma.R under Jons mapp på MONA.
  # Data har hämtats av Samuel Gök i Värmland
  
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
  fil_dalarna <- paste0(input_mapp,"sarbarhet_dalarna_2022.xlsx")
  fil_varmland <- paste0(input_mapp,"sarbarhet_varmland_2022.xlsx")
  fil_gavleborg <- paste0(input_mapp,"sarbarhet_gavleborg_2022.xlsx")
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  ta_med_logga <- FALSE   # FALSE
  
  gg_list <- list()
  
  # ============================= diagram 14 ====================================
  dalarna_df <- read.xlsx(fil_dalarna, sheet = 2) %>% 
    mutate(Län = "Dalarna")
  
  varmland_df <- read.xlsx(fil_varmland, sheet = 2) %>%
    mutate(Län = "Värmland") %>% 
      filter(LA_namn != "Örebro")
  
  gavleborg_df <- read.xlsx(fil_gavleborg, sheet = 2) %>%
    mutate(Län = "Gävleborg")
  
  sarbarhet_df <- rbind(dalarna_df, varmland_df, gavleborg_df) %>% 
    rename(Antal = cnum)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sarbarhet_df", sarbarhet_df, envir = .GlobalEnv)
  }
  
  # UVAS_df$Län <- factor(UVAS_df$Län, 
  #                       levels = c("Dalarna", "Gävleborg", "Värmland"))
  
  diagramfilnamn <- "sarbarhet_ftg.png"
  
  
  # Uppdelat på overrep_df
  gg_obj <- SkapaStapelDiagram(skickad_df = sarbarhet_df,
                               skickad_x_var = "LA_namn",
                               skickad_y_var = "Antal",
                               #skickad_x_grupp = "Län",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "Antal företag",
                               x_axis_sort_value = TRUE,
                               diagram_facet = TRUE,
                               facet_grp = "Län",
                               facet_scale = "free",
                               facet_kolumner = 2, 
                               manual_color = diag_fargvekt[1],
                               skriv_till_diagramfil = spara_diagrambildfil,
                               lagg_pa_logga = ta_med_logga)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c("sarbarhet_ftg")
  
  return(gg_list)
  
}
