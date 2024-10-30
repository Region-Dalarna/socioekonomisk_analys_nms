library(pxweb)
library(httr)
library(dplyr)
library(tidyr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

# ========================================== Inställningar ============================================

mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

region_filter <- hamtaAllaLan()

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

px_df <- NULL
url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/BefSyssAldKonKN"

# hämta senaste år
sen_ar <- hamta_senaste_tid_i_tabell(url_uttag, "år")

varlista <- list(
  Region = region_filter,
  Sysselsattning = '*',
  Alder = '*',
  Kon = '*',
  ContentsCode = "*",
  Tid = sen_ar
)


# API-uttaget
px_uttag <- pxweb_get(url = url_uttag, query = varlista) 


# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region))  
px_df <- px_df %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

syss_df <- px_df %>% 
  mutate(lansnamn = skapa_kortnamn_lan(region)) %>% 
  relocate(lansnamn, .after = region)
names(syss_df)[ncol(syss_df)] <- "syss"

syss_aggr <- syss_df %>% 
  filter(!ålder %in% c("16-19 år", "65-69 år", "70-74 år", "75+ år")) %>%   # ta bort alla åldrar som inte är 20-64 år
  group_by(år, lansnamn, sysselsättning, kön) %>% 
  summarise(syss = sum(syss)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sysselsättning, values_from = syss) %>% 
  mutate(andel = förvärvsarbetande / (förvärvsarbetande + `ej förvärvsarbetande`))

nms_aggr <- syss_aggr %>% 
  filter(lansnamn %in% c("Dalarna", "Gävleborg", "Värmland")) %>% 
  group_by(år, kön) %>% 
  summarise(förvärvsarbetande = sum(förvärvsarbetande),
            `ej förvärvsarbetande` = sum(`ej förvärvsarbetande`)) %>% 
  mutate(lansnamn = "Norra Mellansverige",
         andel = förvärvsarbetande / (`ej förvärvsarbetande` + förvärvsarbetande))

syss_aggr <- syss_aggr %>% 
  bind_rows(nms_aggr) %>% 
  select(-förvärvsarbetande, -`ej förvärvsarbetande`) %>% 
  mutate(andel = round(andel *100,1))

syss_aggr <- syss_aggr %>% 
  mutate(fokus = case_when(lansnamn %in% c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige") & kön == "män" ~ 3,
                           lansnamn %in% c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige") & kön == "kvinnor" ~ 2,
                           !lansnamn %in% c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige") & kön == "män" ~ 1,
                           !lansnamn %in% c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige") & kön == "kvinnor" ~ 0))

# =========================== Skapa diagram ==============================



# Diagram med jämförelse mellan länen för senaste år
diagramfil <- "diagram_13_syss_rams_andel.png"

SkapaStapelDiagram(skickad_df = syss_aggr, 
                   skickad_x_var = "lansnamn", 
                   skickad_y_var = "andel", 
                   skickad_x_grupp = "kön",
                   manual_color = diagramfarger("kon"),
                   x_axis_lutning = 0,
                   diagram_liggande = TRUE,
                   x_axis_sort_value = TRUE,
                   y_axis_100proc = TRUE,
                   manual_y_axis_title = "procent",
                   facet_legend_bottom = TRUE,
                   lagg_pa_logga = FALSE,
                   #x_var_fokus = "fokus",
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   output_mapp = mapp,
                   filnamn_diagram = diagramfil)

 

# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")
