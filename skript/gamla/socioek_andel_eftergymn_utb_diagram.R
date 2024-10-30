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

uttag_regionkoder <- c("00", "17", "20", "21")
uttag_ar <- "*"
valt_ar <- NA            # om NA tas det senaste tillgängliga året

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

# =================================== inställningar för API-uttag ==========================================

url2 <- "/OV0104/v1/doris/sv/ssd/UF/UF0506/UtbBefRegionR"
url1 <- "https://api.scb.se"
url3 <- paste0(url1, url2)

if (is.na(valt_ar)) uttag_ar <- as.character(hamta_senaste_tid_i_tabell(url3, "år")) else uttag_ar <- as.character(valt_ar)

varlista <- list(
  Region = uttag_regionkoder,
  UtbildningsNiva = "*",
  Kon = c("1", "2"),
  Alder = "*",
  ContentsCode = "000000I2",
  Tid = uttag_ar)

# ==========================================================================


px_uttag <- pxweb_get(url = url3,
                      query = varlista) 


# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

px_df$åldernum <- suppressWarnings(parse_number(px_df$ålder))

# döp om bef till antal (det blir enklare så) gör kortnamn av länsnamn (ta bort "s län") och lägg ihop forskarutbildning med övriga eftergymnasiala utbildningar mer än 3 år
utbniva <- px_df %>%
  filter(between(as.numeric(åldernum), 25, 64)) %>% 
  rename(antal = `Befolkning 16-95+ år`) %>% 
  mutate(lansnamn = skapa_kortnamn_lan(region)) %>% 
  mutate(utbildningsnivå = ifelse(utbildningsnivå == "forskarutbildning", "eftergymnasial utbildning, 3 år eller mer", utbildningsnivå))

# lägg ihop till Norra Mellansverige i egen df
utbniva_nms <- utbniva %>% 
  filter(regionkod != "00") %>% 
  group_by(utbildningsnivå, kön, år) %>% 
  summarise(antal = sum(antal)) %>% 
  mutate(regionkod = "xxx",
         region = "Norra Mellansverige", 
         lansnamn = "Norra Mellansverige")

# lägg till NMS till län och riket-df:n
utbniva_tot <- utbniva %>% 
  bind_rows(utbniva_nms)

# grupper ihop
utbniva_tot2 <- utbniva_tot %>% 
  group_by(år, lansnamn, kön, utbildningsnivå) %>% 
  summarise(antal = sum(antal)) %>% 
  mutate(andel = antal / sum(antal))

# döp om värdena på utbildningsnivåerna vi använder
utbniva_tot2$utbildningsnivå[utbniva_tot2$utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"] <- "Eftergymn 3+ år"
utbniva_tot2$utbildningsnivå[utbniva_tot2$utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år"] <- "Eftergymn < 3 år"

# lägg ihop utbildningsnivå och kön till en variabel, filtrera ut bara eftergymnasial utbildning
utbniva_tot3 <- utbniva_tot2 %>% 
  filter(str_detect(utbildningsnivå, "Eftergymn")) %>% 
  mutate(kön = tolower(kön),
         procent = round(andel * 100, 1)) %>% 
  unite("grupp", c(utbildningsnivå, kön), remove = FALSE, sep = " ")

utbniva_tot3$lansnamn <- factor(utbniva_tot3$lansnamn, levels = c("Dalarna", "Gävleborg", "Värmland", "Norra Mellansverige", "Riket"))

dia_filnamn <- "diagram_4_utbniva.png"

SkapaStapelDiagram(skickad_df = utbniva_tot3,
                  skickad_x_var = "lansnamn",
                  skickad_y_var = "procent",
                  skickad_x_grupp = "grupp",
                  diagram_titel = NULL,
                  filnamn_diagram = dia_filnamn,
                  diagram_capt = NULL,
                  output_mapp = mapp,
                  manual_x_axis_text_vjust = 1,
                  #manual_x_axis_text_hjust = 1,
                  x_axis_lutning = 0,
                  #y_axis_borjar_pa_noll = FALSE,
                  manual_y_axis_title = "procent",
                  #manual_x_axis_title = "år",
                  lagg_pa_logga = FALSE,
                  manual_color = diagramfarger("Kön_alla")[3:6])



# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")
