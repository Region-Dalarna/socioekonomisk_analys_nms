library(dplyr)
library(tidyr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
#output_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"


input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
innovationsindex_fil <- "innovationsindex.csv"
innovationsindex_full <- paste0(input_mapp, innovationsindex_fil)

diagram_capt <- "Källa: Reglab\nBearbetning: Samhällsanalys, Region Dalarna"
ta_med_logga <- TRUE    # FALSE

# ============================= diagram 14 ====================================
innind <- read.csv(innovationsindex_full, encoding = "UTF-8")

names(innind)[1] <- "År"

innind <- innind %>% 
  mutate(fokus = ifelse(Region %in% c("Dalarna"), 1,0))
  #mutate(fokus = ifelse(Region %in% c("Dalarna", "Gävleborg", "Värmland"), 1,0))

diagramfilnamn <- "innovationsindex.png"
diagramtitel <- paste0("Reglabs innovationsindex år ", max(innind$År)) 
                                                            
SkapaStapelDiagram(skickad_df = innind %>% filter(År == max(År)),
                   skickad_x_var = "Region",
                   skickad_y_var = "Index",
                   skickad_x_grupp = NA,
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "innovationsindex",
                   x_axis_sort_value = TRUE,
                   x_var_fokus = "fokus",
                   diagram_capt = diagram_capt,
                   diagram_titel = diagramtitel,
                   manual_color = diagramfarger("gron_tva_fokus_morkgron"),
                   lagg_pa_logga = ta_med_logga)
