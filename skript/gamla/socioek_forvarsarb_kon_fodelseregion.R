library(pxweb)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)


source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

# ========================================== Inställningar ============================================

output_mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

uttag_regionkoder <- c("17", "20", "21")
uttag_ar <- "*"
valt_ar <- NA            # om NA tas det senaste tillgängliga året

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

# =================================== inställningar för API-uttag ==========================================

url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
              "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1RikKonUtb")

px_alla <- NULL          # initiera px_alla som tom

qlist <- list(
  Region = "20",
  Kon = "1+2",
  UtbNiv = "000",
  BakgrVar = c("SE"),
  ContentsCode = "0000001X",
  Tid = "*"
)
if (is.na(valt_ar)) uttag_ar <- as.character(hamta_senaste_tid_i_tabell(url_list[1], "år", query_list = qlist)) else uttag_ar <- as.character(valt_ar)

varlista <- list(
  Region = uttag_regionkoder,
  Kon = c("1", "2"),
  UtbNiv = c("000"),
  BakgrVar = c("SE","NEXS","EUEESXN","VXEUEES"),
  ContentsCode = "0000001X",
  Tid = uttag_ar
)

# ==========================================================================


for (tab in 1:length(url_list)){
  
  # byt ut vissa värdlen i varlista när vi tar ut ur riks-tabellen
  if (tab == 2) {
    varlista$ContentsCode <- "000001R8"
    varlista$Region <- "00"
  }
  
  px_uttag <- pxweb_get(url = url_list[tab],
                      query = varlista) 
  
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

  px_alla <- bind_rows(px_alla, px_df)

}


df_af <- px_alla %>%
  mutate(lansnamn = skapa_kortnamn_lan(region),
         kön = tolower(kön)) %>% 
  unite("grupp", c(lansnamn, kön), remove = FALSE, sep = "\n")

df_af$grupp <- factor(df_af$grupp, levels = c("Dalarna\nkvinnor", "Dalarna\nmän",
                                                    "Gävleborg\nkvinnor", "Gävleborg\nmän",
                                                    "Värmland\nkvinnor", "Värmland\nmän",
                                                    "Norra Mellansverige\nkvinnor", "Norra Mellansverige\nmän", 
                                                    "Riket\nkvinnor", "Riket\nmän")) 

df_af <- df_af %>% 
  mutate(bakgrundsvariabel = str_remove(bakgrundsvariabel, "födelseregion: ")) %>% 
  rename(andel = `Andel förvärvsarbetande (ny definition från och med 2019)`)

df_af$bakgrundsvariabel <- factor(df_af$bakgrundsvariabel, 
                                  levels = c("Sverige",
                                             "Norden exkl. Sverige",
                                             "EU/EFTA exkl. Norden",
                                             "övriga världen" )) 

diagramfilnamn <- "diagram_20_forvarvsfrekv_kon_fodelseregion.png"

SkapaStapelDiagram(skickad_df = df_af,
                   skickad_x_var = "grupp",
                   skickad_y_var = "andel",
                   skickad_x_grupp = "bakgrundsvariabel",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "procent",
                   x_axis_lutning = 0,
                   #brew_palett = "Paired",
                   procent_0_100_10intervaller = TRUE,
                   manual_color = rev(diagramfarger("gron_sex")),
                   lagg_pa_logga = FALSE)
  

