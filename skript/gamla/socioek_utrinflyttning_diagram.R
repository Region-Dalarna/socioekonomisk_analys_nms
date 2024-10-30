library(pxweb)
library(httr)
#library(writexl)
#library(data.table)
library(dplyr)
library(tidyr)
library(readr)



source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)


# ========================================== Inställningar ============================================

mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"

uttag_regionkoder <- c("17", "20", "21")
uttag_ar <- "*"
valt_ar <- NA            # om NA tas det senaste tillgängliga året

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

# =================================== inställningar för API-uttag ==========================================

url2 <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101G/BefforandrKvRLK"
url1 <- "https://api.scb.se"
url3 <- paste0(url1, url2)

if (is.na(valt_ar)) uttag_ar <- as.character(hamta_senaste_tid_i_tabell(url3, "år")) else uttag_ar <- as.character(valt_ar)

varlista <- list(
  Region = uttag_regionkoder,
  Forandringar = c("175"),
  Period = "hel",
#  Kon = c("1", "2"),
  ContentsCode = "000002Z9",
  Tid = "*")

# ==========================================================================


px_uttag <- pxweb_get(url = url3,
                      query = varlista) 


# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

utr_inflyttning <- px_df

utr_inflyttning$lansnamn <- skapa_kortnamn_lan(px_df$region)

dia_filnamn <- "diagram_3_utr_inflyttning.png"


SkapaLinjeDiagram(skickad_df = utr_inflyttning,
                  skickad_x_var = "år",
                  skickad_y_var = "Befolkningsstatistik antal personer",
                  skickad_x_grupp = "lansnamn",
                  diagram_titel = NULL,
                  filnamn_diagram = dia_filnamn,
                  diagram_capt = NULL,
                  output_mapp = mapp,
                  #y_axis_borjar_pa_noll = FALSE,
                  manual_y_axis_title = "antal inflyttare från utlandet",
                  #manual_x_axis_title = "år",
                  lagg_pa_logga = FALSE,
                  manual_color = diagramfarger("bla_gra_tre"))



# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")
