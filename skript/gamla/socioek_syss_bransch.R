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
nyckelmapp <- "G:/skript/nycklar/"
branschnyckelfil <- "Bransch_Gxx_farger.csv"

region_filter <- c("20", "17", "21")

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

px_df <- NULL
url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN"

# hämta senaste år
sen_ar <- hamta_senaste_tid_i_tabell(url_uttag, "år")

varlista <- list(
  Region = region_filter,
  SNI2007 = '*',
  Kon = '*',
  ContentsCode = "*",
  Tid = sen_ar
)


# API-uttaget
px_uttag <- pxweb_get(url = url_uttag, query = varlista) 

# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region, SNI2007))  

px_df <- px_df %>% rename(regionkod = Region, branschkod = SNI2007) %>% 
  relocate(regionkod, .before = region) %>% 
  relocate(branschkod, .before = `näringsgren SNI 2007`)

branschnyckel <- read.csv2(paste0(nyckelmapp, branschnyckelfil))

bransch_df <- left_join(px_df, branschnyckel %>% select(Br15kod, Bransch), by = c("branschkod" = "Br15kod"))

bransch_aggr <- bransch_df %>% 
  group_by(år, kön, Bransch) %>% 
  summarise(syss = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`, na.rm = TRUE)) %>% 
  mutate(andel = round((syss / sum(syss))*100,1))

# =========================== Skapa diagram ==============================



# Diagram med jämförelse mellan länen för senaste år
diagramfil <- "diagram_27_syss_bransch_kon.png"

SkapaStapelDiagram(skickad_df = bransch_aggr,
                   skickad_x_var = "Bransch",
                   skickad_y_var = "andel",
                   skickad_x_grupp = "kön",
                   manual_color = diagramfarger("kon"),
                   x_axis_lutning = 0,
                   diagram_liggande = TRUE,
                   x_axis_sort_value = TRUE,
                   #y_axis_100proc = TRUE,
                   #geom_position_stack = TRUE,
                   manual_y_axis_title = "procent",
                   facet_legend_bottom = TRUE,
                   #x_var_fokus = "fokus",
                   #manual_x_axis_text_vjust = 1,
                   #manual_x_axis_text_hjust = 1,
                   lagg_pa_logga = FALSE,
                   output_mapp = mapp,
                   filnamn_diagram = diagramfil)

 

# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")
