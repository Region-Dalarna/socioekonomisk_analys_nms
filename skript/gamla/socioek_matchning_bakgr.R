library(dplyr)
#library(tidyr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

data_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/data_mars2022/"
data_fil <- paste0(data_mapp, "matchning_nms.csv")

matchning_df <- read.csv(data_fil) %>% 
  select(-"X") %>% 
  filter(Ar == max(Ar)) %>% 
  mutate(andel = andel * 100)

matchning_df$matchning_utbniva <- factor(matchning_df$matchning_utbniva, 
                                  levels = c("Utbildningsnivån är för låg",
                                             "Utbildningsnivån är rätt",
                                             "Utbildningsnivån är för hög")) 

diagramfilnamn <- "diagram_21_matchning_bakgr.png"

SkapaStapelDiagram(skickad_df = matchning_df,
                   skickad_x_var = "matchning_utbniva",
                   skickad_y_var = "andel",
                   skickad_x_grupp = "bakgr",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "procent",
                   x_axis_lutning = 0,
                   #brew_palett = "Paired",
                   procent_0_100_10intervaller = TRUE,
                   manual_color = rev(diagramfarger("gron_sex")[c(3,5)]),
                   lagg_pa_logga = FALSE)


