
skapa_befutv_diagram <- function(region_vekt = c("17", "20", "21"),
                                 valt_ar = NA,            # om NA tas det senaste tillgängliga året
                                 diagbild_bredd = 16,
                                 diagbild_hojd = 9,
                                 spara_diagrambild = FALSE
                                 ){
  library(pxweb)
  library(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  
  
  url2 <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  url1 <- "https://api.scb.se"
  url3 <- paste0(url1, url2)
  
  if (is.na(valt_ar)) uttag_ar <- as.character(max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))) else uttag_ar <- as.character(valt_ar)
  
  varlista <- list(
    Region = region_vekt,
    Kon = c('*'),
    ContentsCode = "BE0101N1",
    Tid = as.character(c(2010:uttag_ar)))
  
  # ==========================================================================
  
  
  px_uttag <- pxweb_get(url = url3,
                        query = varlista) 
  
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  px_df$lansnamn <- skapa_kortnamn_lan(px_df$region)
  
  bef_antal <- px_df %>% 
    group_by(år, lansnamn) %>% 
    summarise(antal = sum(Folkmängd)) %>% 
    ungroup()
  
  #bef_antal$antal <- format(bef_antal$antal, big.mark = " ")
  
  dia_titel <- ""
  dia_filnamn <- "diagram2_befutv.png"
  
  
  befutv_linjediagram <- SkapaLinjeDiagram(skickad_df = bef_antal,
                    skickad_x_var = "år",
                    skickad_y_var = "antal",
                    skickad_x_grupp = "lansnamn",
                    diagram_titel = NULL,
                    filnamn_diagram = dia_filnamn,
                    diagram_capt = NULL,
                    output_mapp = mapp,
                    y_axis_borjar_pa_noll = FALSE,
                    manual_y_axis_title = "invånare",
                    manual_x_axis_title = "år",
                    lagg_pa_logga = FALSE,
                    manual_color = diagramfarger("bla_gra_tre"),
                    diagramfil_bredd = diagbild_bredd,
                    diagramfil_hojd = diagbild_hojd,
                    skriv_till_diagramfil = spara_diagrambild
                    )

  
  return(befutv_linjediagram)
  
} # slut funktion
