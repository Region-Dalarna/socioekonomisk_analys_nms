library(pxweb)
#library(httr)
#library(writexl)
#library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(gt)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

# ========================================== Inställningar ============================================

skapa_bef_tabell <- function(region_vekt = c("00", "17", "20", "21"),
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

gt_tbl <- gt(bef_klar) %>%
  # Justera tabellens bredd och kolumners bredd
  cols_width(
    everything() ~ px(150)  # justera px-värdet efter behov för att fylla bredden
  ) %>%
  # Justera linjerna så att lodräta linjer tas bort
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid",  # behåll en linje under rubrikerna
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "grey",
    table_body.border.top.style = "none",
    table_body.border.bottom.style = "none"
  ) %>%
  # rad 1-4, kol 2-5, själva värdecellerna
  tab_style(
    style = list(
      cell_text(
        align = "right"
      )
    ),
    locations = list(
      cells_body(
        rows = 1:4,
        columns = 2:5
      )
    )
  ) %>%
  # fetstila kolumn 1 som egentligen är labels
  tab_style(
    style = list(
      cell_text(
        weight = "bold"
      )
    ),
    locations = list(
      cells_body(
        rows = 1:4,
        columns = 1
      )
    )
  ) %>%
  # få till länsrubrikerna
  tab_style(
    style = list(
      cell_text(
        weight = "bold",
        align = "right"
      )
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>%
  # rita tjock linje under Antal
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "grey",
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        rows = 1
      )
    )
  )

# Spara tabellen som en bild
gtsave(gt_tbl, "bef_tabell.png")

return(gt_tbl)
}
