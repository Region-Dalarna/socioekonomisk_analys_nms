library(pxweb)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
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

url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003E/IntGr3LanKONS",
              "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003E/IntGr3RikKONS")

px_alla <- NULL          # initiera px_alla som tom

qlist <- list(
  Region = "20",
  Kon = "1+2",
  Bakgrund = c("EUp"),
  ContentsCode = "0000016Z",
  Tid = "*"
)
if (is.na(valt_ar)) uttag_ar <- as.character(hamta_senaste_tid_i_tabell(url_list[1], "år", query_list = qlist)) else uttag_ar <- as.character(valt_ar)

varlista <- list(
  Region = uttag_regionkoder,
  Kon = c("1", "2"),
  Bakgrund = "EUp",
  ContentsCode = c("0000016Z","00000170","00000171","00000172"),
  Tid = uttag_ar
)

# ==========================================================================


for (tab in 1:length(url_list)){
  
  # byt ut vissa värdlen i varlista när vi tar ut ur riks-tabellen
  if (tab == 2) {
    varlista$ContentsCode <- c("0000017Z","00000180","00000181","00000182")
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


df_dia <- px_alla %>%
  pivot_longer(6:9, names_to = "fodelseregion", values_to = "andel") %>% 
  mutate(lansnamn = skapa_kortnamn_lan(region),
         kön = tolower(kön),
         fodelseregion = str_remove(fodelseregion, "Födda i "),
         fodelseregion_radbryt = dela_upp_strang_radbryt(fodelseregion, max_langd = 10))


df_dia$fodelseregion_radbryt <- factor(df_dia$fodelseregion_radbryt, 
                                  levels = unique(df_dia$fodelseregion_radbryt))

df_dia$lansnamn <- factor(df_dia$lansnamn, levels =
                            c("Dalarna",
                              "Gävleborg",
                              "Värmland",
                              "Riket"))

diagramfilnamn <- "diagram_7_eftergymnutb_kon_fodelseregion.png"

SkapaStapelDiagram(skickad_df = df_dia,
                   skickad_x_var = "fodelseregion_radbryt",
                   skickad_y_var = "andel",
                   skickad_x_grupp = "kön",
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   manual_y_axis_title = "procent",
                   facet_legend_bottom = TRUE,
                   x_axis_lutning = 0,
                   diagram_facet = TRUE,
                   facet_x_axis_storlek = 10.5,
                   facet_y_axis_storlek = 12,
                   facet_scale = "fixed",
                   facet_grp = "lansnamn",
                   procent_0_100_10intervaller = TRUE,
                   manual_color = diagramfarger("kon"),
                   lagg_pa_logga = FALSE)
  

