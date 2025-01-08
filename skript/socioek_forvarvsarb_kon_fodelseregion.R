skapa_forvarsarb_kon_fodelseregion <- function(region_vekt = c("17", "20", "21"),
                                                      spara_diagrambildfil = FALSE,
                                                      diag_fargvekt = NA,
                                                      returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar 
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("bla_gra_fyra")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  uttag_regionkoder <- region_vekt
  uttag_ar <- "*"
  valt_ar <- NA            # om NA tas det senaste tillgängliga året
  
  # =================================== inställningar för API-uttag ==========================================
  
  # Tidigare
  # url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
  #               "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1RikKonUtb")
  
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS",
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1RikUtbBAS")
  
  px_alla <- NULL          # initiera px_alla som tom
  
  qlist <- list(
    Region = "20",
    Kon = "1+2",
    UtbNiv = "000",
    BakgrVar = c("SE"),
    #ContentsCode = "0000001X",
    ContentsCode = "000007KE",
    Tid = "*"
  )
  
  #hamta_senaste_tid_i_tabell(url_list[1], "år", query_list = qlist)
  if (is.na(valt_ar)) uttag_ar <- as.character(max(hamta_giltiga_varden_fran_tabell(url_list[1], "tid"))) else uttag_ar <- as.character(valt_ar)
  
  varlista <- list(
    Region = region_vekt,
    Kon = c("1", "2"),
    UtbNiv = c("000"),
    BakgrVar = c("SE","NEXS","EUEESXN","VXEUEES"),
    #ContentsCode = "0000001X",
    ContentsCode = "000007KE",
    Tid = uttag_ar
  )
  
  # ==========================================================================
  
  
  for (tab in 1:length(url_list)){
    
    # byt ut vissa värdlen i varlista när vi tar ut ur riks-tabellen
    if (tab == 2) {
      varlista$ContentsCode <- "000007JE"
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
    rename(andel = `Andel sysselsatta`)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("forvarvsarbetande_kon_fodelseregion_df", df_af, envir = .GlobalEnv)
  }
  
  df_af$bakgrundsvariabel <- factor(df_af$bakgrundsvariabel, 
                                    levels = c("Sverige",
                                               "Norden exkl. Sverige",
                                               "EU/EFTA exkl. Norden",
                                               "övriga världen" )) 
  
  diagramfilnamn <- "diagram_20_forvarvsfrekv_kon_fodelseregion.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df_af,
                               skickad_x_var = "grupp",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "bakgrundsvariabel",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_y_axis_title = "procent",
                               x_axis_lutning = 0,
                               procent_0_100_10intervaller = TRUE,
                               #manual_color = rev(diagramfarger("gron_sex")),
                               manual_color = diag_fargvekt,
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  return(gg_obj)
  
}
