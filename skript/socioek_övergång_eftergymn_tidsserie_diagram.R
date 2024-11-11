skapa_overgang_eftergym_studier_tidsserie <- function(region_vekt = c("00", "17", "20", "21"),
                                            spara_diagrambildfil = FALSE,
                                            returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar 
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  uttag_regionkoder <- region_vekt
  
  uttag_ar <- "*"
  valt_ar <- NA            # om NA tas det senaste tillgängliga året
  
  # =============================================== API-uttag ===============================================
  
  # fyll lista som vi loopar igenom
  
  # Contentscode innan SCB gjorde en förändring
  # tab_list <- list(
  #   url = list(
  #     "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542B/UF0542T2A",
  #     "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542C/UF0542T3A",
  #     "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542D/UF0542T4A"),
  #   
  #   contcode = list(c("000004GD", "000004GG"),
  #                   c("000004H4", "000004H7"),
  #                   c("000004FR", "000004I0")),
  #   
  #   kolumnnamn = list("Högskola", "Yrkeshögskola", "Folkhögskola")
  # )
  
  # Nytt, förändring eftera att SCB ändrat adress (för övergång till högskola)
  tab_list <- list(
    url = list(
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542B/UF0542T2AN",
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542C/UF0542T3A",
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542D/UF0542T4A"),

    contcode = list(c("000007NR", "000007NT"),
                    c("000004H4", "000004H7"),
                    c("000004FR", "000004I0")),

    kolumnnamn = list("Högskola", "Yrkeshögskola", "Folkhögskola")
  )
  
  pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0542/UF0542B/UF0542T2AN", "contentscode")
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
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("overgang_eftergymn_tidserie_df", overg_tidsserie, envir = .GlobalEnv)
  }
  
  overg_tidsserie$lansnamn <- factor(overg_tidsserie$lansnamn, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket"))
  
  dia_filnamn <- "diagram_12_övergång_högsk_tidsserie.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = overg_tidsserie,
                               skickad_x_var = "avgångsår",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "kön",
                               facet_grp = "lansnamn",
                               diagram_facet = TRUE,
                               facet_legend_bottom = TRUE,
                               facet_scale = "fixed",
                               diagram_titel = NULL,
                               filnamn_diagram = dia_filnamn,
                               diagram_capt = NULL,
                               output_mapp = mapp,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_lutning = 45,
                               #y_axis_borjar_pa_noll = FALSE,
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               #diagram_liggande = TRUE,
                               manual_color = diagramfarger("kon"),
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  
  
  
  return(gg_obj)
  
}
