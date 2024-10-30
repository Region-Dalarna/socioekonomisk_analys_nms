library(pxweb)
library(httr)
library(dplyr)
library(tidyr)
library(readxl)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

output_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"
nyckel_mapp <- "G:/skript/nycklar/"
nyckel_fil <- paste0(nyckel_mapp, "Bransch_FEK.xlsx")

region_filter <- c("17", "20", "21")

nyckel_df <- read_xlsx(nyckel_fil)


#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================


px_df <- NULL
url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0109/NV0109L/RegionalBasf07"

# hämta senaste år
sen_ar <- hamta_senaste_tid_i_tabell(url_uttag, "år")

varlista <- list(
  Region = region_filter,
  SNI2007 = '*',
  ContentsCode ='*',
  Tid = sen_ar
)


# API-uttaget
px_uttag <- pxweb_get(url = url_uttag, query = varlista) 


# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region, SNI2007))  

px_df <- px_df %>% 
  rename(regionkod = Region, branschkod = SNI2007) %>% 
  relocate(regionkod, .before = region) %>% 
  relocate(branschkod, .before = `näringsgren SNI 2007`) %>% 
  left_join(nyckel_df %>% select(Kod, Branschgrupp), by = c("branschkod" = "Kod"))

varde_df <- px_df %>% 
  group_by(Branschgrupp) %>% 
  summarise(intakter = sum(`Totala Intäkter, mnkr`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(intakter > 0)



diagramfilnamn <- "diagram_46_intakter.png"

SkapaStapelDiagram(skickad_df = varde_df,
                   skickad_x_var = "Branschgrupp",
                   skickad_y_var = "intakter",
                   skickad_x_grupp = NA,
                   x_axis_sort_value = TRUE,
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "totala intäkter, mkr",
                   x_axis_lutning = 45,
                   #brew_palett = "Paired",
                   #manual_color = rev(diagramfarger("gron_sex")[c(3,5)]),
                   lagg_pa_logga = FALSE)

arbst_df <- px_df %>% 
  group_by(Branschgrupp) %>% 
  summarise(arbst = sum(`Antal arbetsställen`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(arbst > 0)



diagramfilnamn <- "diagram_47_arbetsstallen.png"

SkapaStapelDiagram(skickad_df = arbst_df,
                   skickad_x_var = "Branschgrupp",
                   skickad_y_var = "arbst",
                   skickad_x_grupp = NA,
                   x_axis_sort_value = TRUE,
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "antal arbetsställen",
                    x_axis_lutning = 45,
                   #brew_palett = "Paired",
                   #manual_color = rev(diagramfarger("gron_sex")[c(3,5)]),
                   lagg_pa_logga = FALSE)

