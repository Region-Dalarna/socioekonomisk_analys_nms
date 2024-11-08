library(tidyverse)
library(openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

output_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

input_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
af_fil <- "underlag uppdatering AF 2022.xlsx"
af_full <- paste0(input_mapp, af_fil)

# ============================= diagram 14 ====================================
af_dia14 <- read.xlsx(af_full, sheet = "diagram14", startRow = 3)

af_dia14 <- af_dia14 %>%
  mutate(Kön = tolower(Kön)) %>% 
  unite("grupp", c(Län, Kön), remove = FALSE, sep = "\n")

af_dia14$grupp <- factor(af_dia14$grupp, levels = c("Dalarna\nkvinnor", "Dalarna\nmän",
                                                    "Gävleborg\nkvinnor", "Gävleborg\nmän",
                                                    "Värmland\nkvinnor", "Värmland\nmän",
                                                    "Norra Mellansverige\nkvinnor", "Norra Mellansverige\nmän", 
                                                    "Riket\nkvinnor", "Riket\nmän")) 

af_dia14$Längd <- factor(af_dia14$Längd, levels = c("< 6 månader", "6-12 månader", "> 12 månader"))
af_dia14$proc <- af_dia14$Andel * 100


diagramfilnamn <- "diagram_14_andel_arblosa_arbtid.png"
                                                            
SkapaStapelDiagram(skickad_df = af_dia14,
                   skickad_x_var = "grupp",
                   skickad_y_var = "proc",
                   skickad_x_grupp = "Längd",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   geom_position_stack = TRUE,
                   manual_y_axis_title = "procent",
                   manual_color = diagramfarger("gron_sex")[c(3,4,5)],
                   lagg_pa_logga = FALSE)

# ============================= diagram 15 ====================================

af_nr <- "15"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df <- af_dia_df %>%
  mutate(tid = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y"))

af_dia_df <- af_dia_df %>%
  arrange(År, Månad) %>% 
  mutate(sortvar = row_number())

af_dia_df$tid <- reorder(af_dia_df$tid, af_dia_df$sortvar)


diagramfilnamn <- paste0("diagram_", af_nr, "_varslade_Sverige.png")

SkapaStapelDiagram(skickad_df = af_dia_df,
                   skickad_x_var = "tid",
                   skickad_y_var = "Antal varslade",
                   skickad_x_grupp = NA,
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   x_axis_lutning = 90,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   #manual_color = diagramfarger("gron_sex")[c(3,4,5)],
                   lagg_pa_logga = FALSE)
 
# af_dia_df <- af_dia_df %>%
#   mutate(tid_txt = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y")) %>% 
#   arrange(sortvar)

# filnamntest <- "test.png"
# SkapaLinjeDiagram(skickad_df = af_dia_df,
#                    skickad_x_var = "tid_txt",
#                    skickad_y_var = "Antal varslade",
#                    output_mapp = output_mapp,
#                    filnamn_diagram = filnamntest,
#                    x_axis_lutning = 90,
#                    #manual_x_axis_text_vjust = 1,
#                    #manual_x_axis_text_hjust = 1,
#                    #manual_color = diagramfarger("gron_sex")[c(3,4,5)],
#                    lagg_pa_logga = FALSE)


# ============================= diagram 16 ====================================

af_nr <- "16"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df <- af_dia_df %>%
  mutate(tid = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y"))

af_dia_df <- af_dia_df %>%
  arrange(År, Månad) %>% 
  mutate(sortvar = row_number())

af_dia_df$tid <- reorder(af_dia_df$tid, af_dia_df$sortvar)

diagramfilnamn <- paste0("diagram_", af_nr, "_varslade_NMS.png")

SkapaStapelDiagram(skickad_df = af_dia_df,
                   skickad_x_var = "tid",
                   skickad_y_var = "Antal varslade",
                   skickad_x_grupp = "Län",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   x_axis_lutning = 90,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   manual_color = diagramfarger("bla_gra_tre"),
                   lagg_pa_logga = FALSE)

# ============================= diagram 19 ====================================

af_nr <- "19"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df$År_txt <- as.character(af_dia_df$År)
af_dia_df$Vecka_factor <- factor(af_dia_df$Vecka)

diagramfilnamn <- paste0("diagram_", af_nr, "_nyinskrivna_arbsok_NMS.png")

SkapaLinjeDiagram(skickad_df = af_dia_df,
                   skickad_x_var = "Vecka_factor",
                   skickad_y_var = "Antal",
                   skickad_x_grupp = "År_txt",
                   diagram_facet = TRUE,
                   facet_grp = "Region",
                   facet_legend_bottom = TRUE,
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   x_axis_lutning = 90,
                   visa_var_x_xlabel = 6,
                   x_axis_storlek = 6,
                   manual_x_axis_title = "vecka",
                   manual_y_axis_title = "antal arbetssökande",
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   #manual_color = diagramfarger("bla_gra_tre"),
                   lagg_pa_logga = FALSE)

# ============================= diagram 21 ====================================

af_nr <- "21"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df <- af_dia_df %>%
  mutate(tid = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y"))

af_dia_df <- af_dia_df %>%
  arrange(År, Månad) %>% 
  mutate(sortvar = row_number())

af_dia_df$tid <- reorder(af_dia_df$tid, af_dia_df$sortvar)

af_dia_df$Region <- factor(af_dia_df$Region, 
                           levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 

af_dia_df$proc <- af_dia_df$`Andel arbetslösa` * 100

diagramfilnamn <- paste0("diagram_", af_nr, "_arblosa_NMS_riket.png")

SkapaLinjeDiagram(skickad_df = af_dia_df,
                  skickad_x_var = "tid",
                  skickad_y_var = "proc",
                  skickad_x_grupp = "Region",
                  output_mapp = output_mapp,
                  filnamn_diagram = diagramfilnamn,
                  x_axis_lutning = 90,
                  #visa_var_x_xlabel = 6,
                  #x_axis_storlek = 8,
                  #manual_x_axis_title = "vecka",
                  manual_y_axis_title = "procent",
                  #manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  manual_color = c(diagramfarger("bla_gra_tre"), "black"),
                  lagg_pa_logga = FALSE)

# ============================= diagram 22 ====================================

af_nr <- "22"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df <- af_dia_df %>%
  mutate(tid = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y"))

af_dia_df <- af_dia_df %>%
  arrange(År, Månad) %>% 
  mutate(sortvar = row_number())

af_dia_df$tid <- reorder(af_dia_df$tid, af_dia_df$sortvar)

af_dia_df$Region <- factor(af_dia_df$Region, 
                           levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 

af_dia_df$proc <- af_dia_df$`Andel arbetslösa` * 100

diagramfilnamn <- paste0("diagram_", af_nr, "_arblosa_unga_NMS_riket.png")

SkapaLinjeDiagram(skickad_df = af_dia_df,
                  skickad_x_var = "tid",
                  skickad_y_var = "proc",
                  skickad_x_grupp = "Region",
                  output_mapp = output_mapp,
                  filnamn_diagram = diagramfilnamn,
                  x_axis_lutning = 90,
                  #visa_var_x_xlabel = 6,
                  #x_axis_storlek = 8,
                  #manual_x_axis_title = "vecka",
                  manual_y_axis_title = "procent",
                  #manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  manual_color = c(diagramfarger("bla_gra_tre"), "black"),
                  lagg_pa_logga = FALSE)

# ============================= diagram 24 ====================================

af_nr <- "24"
af_dia_df <- read_xlsx(af_full, sheet = paste0("diagram", af_nr) , skip = 2)

af_dia_df <- af_dia_df %>%
  mutate(tid = format(as.Date(paste(År, Månad, "1", sep = "-")), format = "%b - %y"))

af_dia_df <- af_dia_df %>%
  arrange(År, Månad) %>% 
  mutate(sortvar = row_number())

af_dia_df$tid <- reorder(af_dia_df$tid, af_dia_df$sortvar)

af_dia_df$Region <- factor(af_dia_df$Region, 
                           levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")) 

af_dia_df$proc <- af_dia_df$`Andel arbetslösa` * 100

diagramfilnamn <- paste0("diagram_", af_nr, "_arblosa_utrfodda_NMS_riket.png")

SkapaLinjeDiagram(skickad_df = af_dia_df,
                  skickad_x_var = "tid",
                  skickad_y_var = "proc",
                  skickad_x_grupp = "Region",
                  output_mapp = output_mapp,
                  filnamn_diagram = diagramfilnamn,
                  x_axis_lutning = 90,
                  #visa_var_x_xlabel = 6,
                  #x_axis_storlek = 8,
                  #manual_x_axis_title = "vecka",
                  manual_y_axis_title = "procent",
                  #manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  manual_color = c(diagramfarger("bla_gra_tre"), "black"),
                  lagg_pa_logga = FALSE)
