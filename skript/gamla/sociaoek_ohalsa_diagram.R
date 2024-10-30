library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

output_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
ohalsa_fil <- "underlag uppdatering ohälsa 2022.xlsx"
ohalsa_full <- paste0(input_mapp, ohalsa_fil)


# ============================= diagram 29 ====================================

ohalsa_nr <- "29"
ohalsa_df <- read.xlsx(ohalsa_full, sheet = paste0("diagram", ohalsa_nr) , startRow = 3)

ohalsa_df$Län <- factor(ohalsa_df$Län, 
                           levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 


diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_ohalsotal_kon_NMS_riket.png")

SkapaStapelDiagram(skickad_df = ohalsa_df,
                  skickad_x_var = "Län",
                  skickad_y_var = "Ohälsotal",
                  skickad_x_grupp = "Kön",
                  output_mapp = output_mapp,
                  filnamn_diagram = diagramfilnamn,
                  x_axis_lutning = 0,
                  #visa_var_x_xlabel = 6,
                  #x_axis_storlek = 8,
                  #manual_x_axis_title = "vecka",
                  #manual_y_axis_title = "procent",
                  #manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  manual_color = diagramfarger("kon"),
                  lagg_pa_logga = FALSE)

# ============================= diagram 30 ====================================

ohalsa_nr <- "30"
ohalsa_df <- read_xlsx(ohalsa_full, sheet = paste0("diagram", ohalsa_nr) , skip = 2)

ohalsa_df$Län <- factor(ohalsa_df$Län, 
                        levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 

diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_sjukpenningtal_kon_NMS_riket.png")

SkapaStapelDiagram(skickad_df = ohalsa_df,
                   skickad_x_var = "Län",
                   skickad_y_var = "Sjukpenningtal",
                   skickad_x_grupp = "Kön",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   x_axis_lutning = 0,
                   #visa_var_x_xlabel = 6,
                   #x_axis_storlek = 8,
                   #manual_x_axis_title = "vecka",
                   #manual_y_axis_title = "procent",
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   manual_color = diagramfarger("kon"),
                   lagg_pa_logga = FALSE)

# ============================= diagram 31 ====================================

ohalsa_nr <- "31"
ohalsa_df <- read.xlsx(ohalsa_full, sheet = paste0("diagram", ohalsa_nr) , startRow =  3)

ohalsa_df$Län <- factor(ohalsa_df$Län, 
                        levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 

diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_sjukpenningtal_alder_NMS_riket.png")

SkapaStapelDiagram(skickad_df = ohalsa_df,
                   skickad_x_var = "Ålder",
                   skickad_y_var = "Sjukpenningtal",
                   skickad_x_grupp = "Kön",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   x_axis_lutning = 0,
                   diagram_facet = TRUE,
                   facet_grp = "Län",
                   #visa_var_x_xlabel = 6,
                   #x_axis_storlek = 8,
                   manual_x_axis_title = "åldersgrupp",
                   #manual_y_axis_title = "procent",
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   manual_color = diagramfarger("kon"),
                   lagg_pa_logga = FALSE)

# ============================= diagram 32 ====================================

ohalsa_nr <- "32"
ohalsa_df <- read.xlsx(ohalsa_full, sheet = paste0("diagram", ohalsa_nr) , startRow = 3)

ohalsa_df <- ohalsa_df %>%
  mutate(Kön = tolower(Kön)) %>% 
  unite("grupp", c(Län, Kön), remove = FALSE, sep = "\n")

ohalsa_df$grupp <- factor(ohalsa_df$grupp, levels = c("Dalarna\nkvinnor", "Dalarna\nmän",
                                                    "Gävleborg\nkvinnor", "Gävleborg\nmän",
                                                    "Värmland\nkvinnor", "Värmland\nmän",
                                                    "Norra Mellansverige\nkvinnor", "Norra Mellansverige\nmän", 
                                                    "Riket\nkvinnor", "Riket\nmän")) 
ohalsa_df <- ohalsa_df %>% 
  mutate(proc = Andel * 100)

diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_orsaker_sjukfranvaro.png")

SkapaStapelDiagram(skickad_df = ohalsa_df,
                   skickad_x_var = "grupp",
                   skickad_y_var = "proc",
                   skickad_x_grupp = "Orsak",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #x_axis_lutning = 0,
                   #visa_var_x_xlabel = 6,
                   #x_axis_storlek = 8,
                   #manual_x_axis_title = "åldersgrupp",
                   manual_y_axis_title = "procent",
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   #manual_color = diagramfarger("kon"),
                   y_axis_100proc = TRUE,
                   geom_position_stack = TRUE,
                   lagg_pa_logga = FALSE)

# ============================= diagram 33 ====================================

ohalsa_nr <- "33"
ohalsa_df <- read.xlsx(ohalsa_full, sheet = paste0("diagram", ohalsa_nr) , startRow = 3)

ohalsa_df$Län <- factor(ohalsa_df$Län, 
                        levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 


ohalsa_df <- ohalsa_df %>% 
  mutate(proc = Andel * 100)

diagramfilnamn <- paste0("diagram_", ohalsa_nr, "_sjalvskattad_halsa.png")

SkapaStapelDiagram(skickad_df = ohalsa_df,
                   skickad_x_var = "Län",
                   skickad_y_var = "proc",
                   skickad_x_grupp = "Kön",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #x_axis_lutning = 0,
                   #visa_var_x_xlabel = 6,
                   #x_axis_storlek = 8,
                   #manual_x_axis_title = "åldersgrupp",
                   manual_y_axis_title = "procent",
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   manual_color = diagramfarger("kon"),
                   procent_0_100_10intervaller = TRUE,
                   lagg_pa_logga = FALSE)
