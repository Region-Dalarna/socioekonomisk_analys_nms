library(pxweb)
library(httr)
library(dplyr)
library(tidyr)
library(readr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

# ========================================== Inställningar ============================================

mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

uttag_regionkoder <- c("00", "17", "20", "21")
uttag_ar <- "*"
valt_ar <- NA            # om NA tas det senaste tillgängliga året

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

# fyll lista som vi loopar igenom

tab_list <- list(
  url = list(
  "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542B/UF0542T2A",
  "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542C/UF0542T3A",
  "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542D/UF0542T4A"),
  
  contcode = list(c("000004GD", "000004GG"),
                  c("000004H4", "000004H7"),
                  c("000004FR", "000004I0")),
  
  kolumnnamn = list("Högskola", "Yrkeshögskola", "Folkhögskola")
)

# =================================== inställningar för API-uttag ==========================================

px_alla <- NULL
#if (is.na(valt_ar)) uttag_ar <- as.character(hamta_senaste_tid_i_tabell(url3, "år", region_varde = "20B")) else uttag_ar <- as.character(valt_ar)

# gör speciallösning för denna tabell, lägg på "B" på alla regionkoder utom riket (00)
if ("00" %in% uttag_regionkoder){
  special_regionkoder <- uttag_regionkoder[uttag_regionkoder != "00"]
  special_regionkoder <- paste0(special_regionkoder, "B")
  special_regionkoder <- c("00", special_regionkoder)
} else special_regionkoder <- paste0(uttag_regionkoder, "B")

# här loopar vi igenom tabellistan och gör pxwebb-uttag
for (tab in 1:1){
  # fyll variabellista med rätt contentcode, övrigt är samma i alla tre tabeller
  varlista <- list(
    Region = special_regionkoder,          
    Program = "tot",
    Kon = c("1", "2"),
    Behorighet = "Tot",
    GenomsnittBetygGym = "tot",
    ContentsCode = tab_list[["contcode"]][[tab]],
    Tid = "*")
  
  # här gör vi själva API-uttaget
  px_uttag <- pxweb_get(url = tab_list[["url"]][[tab]], query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% 
    rename(regionkod = Region) %>% 
    relocate(regionkod, .before = region)
  
  px_df <- px_df %>% mutate(utbtyp = tab_list[["kolumnnamn"]][[tab]])

  px_alla <- bind_rows(px_alla, px_df)
  
}

overg_tidsserie <- px_alla %>%
  filter(!is.na(`Antal som läst vidare inom 3 år`)) %>% 
  mutate(andel = round(`Antal som läst vidare inom 3 år`/`Antal gymnasieexaminerade totalt`*100,1),
         lansnamn = trimws(skapa_kortnamn_lan(region)))

overg_tidsserie$lansnamn <- factor(overg_tidsserie$lansnamn, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket"))

dia_filnamn <- "diagram_12_övergång_högsk_tidsserie.png"

SkapaStapelDiagram(skickad_df = overg_tidsserie,
                  skickad_x_var = "avgångsår",
                  skickad_y_var = "andel",
                  skickad_x_grupp = "kön",
                  facet_grp = "lansnamn",
                  diagram_facet = TRUE,
                  facet_legend_bottom = TRUE,
                  diagram_titel = NULL,
                  filnamn_diagram = dia_filnamn,
                  diagram_capt = NULL,
                  output_mapp = mapp,
                  #manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  x_axis_lutning = 0,
                  #y_axis_borjar_pa_noll = FALSE,
                  manual_y_axis_title = "procent",
                  #manual_x_axis_title = "år",
                  lagg_pa_logga = FALSE,
                  #diagram_liggande = TRUE,
                  manual_color = diagramfarger("kon"))



# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")
