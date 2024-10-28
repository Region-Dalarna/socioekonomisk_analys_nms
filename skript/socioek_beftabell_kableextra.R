library(pxweb)
#library(httr)
#library(writexl)
#library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(kableExtra)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

# ========================================== Inställningar ============================================

skapa_bef_tabell_kbl <- function(region_vekt = c("00", "17", "20", "21"),
                             valt_ar = NA){

url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

if (is.na(valt_ar)) uttag_ar <- as.character(max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))) else uttag_ar <- as.character(valt_ar)

varlista <- list(
  Region = region_vekt,
  Alder = '*',
  ContentsCode = "BE0101N1",
  Tid = uttag_ar)

px_uttag <- pxweb_get(url = url_uttag,
                      query = varlista) 

# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

px_df <- px_df %>% 
  mutate(region = region %>% skapa_kortnamn_lan(),
         alder_num = suppressWarnings(parse_number(ålder)),
         aldergrp = skapa_aldersgrupper(alder_num, c(0, 20, 65)),
         aldergrp = as.character(aldergrp), 
         aldergrp = ifelse(is.na(aldergrp), "totalt", aldergrp))


bef_antal <- px_df %>% 
  filter(aldergrp == "totalt") %>%
  mutate(aldergrp = "Antal") %>% 
  group_by(aldergrp, region) %>% 
  summarise(antal = sum(Folkmängd)) %>% 
  ungroup() %>% 
  mutate(antal = format(antal, big.mark = " "))

bef_antal <- bef_antal %>% 
  pivot_wider(id_cols = aldergrp, names_from = region, values_from = antal) %>% 
  relocate(Riket, .after = 5)
  
bef_aggr <- px_df %>% 
  filter(aldergrp != "totalt") %>% 
  group_by(region, aldergrp) %>% 
  summarise(antal = sum(Folkmängd)) %>% 
  mutate(andel = antal / sum(antal)) %>%
  mutate(andel = round(andel *100)) %>% 
  mutate(andel = paste0(as.character(andel), " %")) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = aldergrp, names_from = region, values_from = andel) %>% 
  relocate(Riket, .after = 5)
    
bef_klar <- bef_antal %>% 
  bind_rows(bef_aggr) %>% 
  rename(" " = aldergrp)


# Skapa tabellen med kableExtra
kable_tabell <- library(kableExtra)

# Skapar tabellen utan den övre "Regioner"-raden och justerar bredden
kable_tabell <- bef_klar %>%
  kable("html", align = "r", col.names = c("", "Dalarna", "Gävleborg", "Värmland", "Riket")) %>%
  kable_styling(
    full_width = F,
    bootstrap_options = c("striped"),
    fixed_thead = TRUE
  ) %>%
  column_spec(1, bold = TRUE, width = "5em") %>%                    # Fetstil för första kolumnen
  column_spec(2:5, width = "10em") %>%                              # Justera bredden på övriga kolumner
  row_spec(1, extra_css = "border-bottom: 2px solid grey;") %>%     # Tjockare linje under "Antal"
  row_spec(0, background = "white") %>%                             # Rubrikradens bakgrund till vit
  row_spec(1:nrow(bef_klar), background = "white") %>%              # Sätter alla datarader till vit bakgrund
  row_spec(nrow(bef_klar), extra_css = "border-bottom: 2px solid grey;")



return(kable_tabell)
}
