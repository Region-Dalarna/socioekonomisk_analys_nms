skapa_inskr_arb_kon_alder_utbniva <- function(region_vekt = c("17", "20", "21"),
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
  
  uttag_ar <- "*"
  valt_ar <- NA            # om NA tas det senaste tillgängliga året
  
  # =============================================== API-uttag ===============================================
  
  # =================================== inställningar för API-uttag ==========================================
  
  url2 <- "/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb"
  url1 <- "https://api.scb.se"
  url3 <- paste0(url1, url2)
  
  qlist <- list(
    Region = "20",
    Kon = "1+2",
    UtbNiv = "000",
    BakgrVar = "tot20-64",
    ContentsCode = "000000LS",
    Tid = "*"
  )
  if (is.na(valt_ar)) uttag_ar <- as.character(max(hamta_giltiga_varden_fran_tabell(url3, "tid"))) else uttag_ar <- as.character(valt_ar)
  #hamta_senaste_tid_i_tabell(url3, "år", query_list = qlist
  varlista <- list(
    Region = region_vekt,
    Kon = c("1", "2"),
    UtbNiv = c("000", "F"),
    BakgrVar = c("20-24","25-34"),
    ContentsCode = "000000LS",
    Tid = uttag_ar
  )
  
  # ==========================================================================
  
  
  px_uttag <- pxweb_get(url = url3,
                        query = varlista) 
  
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  px_df$lansnamn <- skapa_kortnamn_lan(px_df$region)
  
  df_af_183 <- px_df %>%
    mutate(kön = tolower(kön)) %>% 
    unite("grupp", c(lansnamn, kön), remove = FALSE, sep = "\n")
  
  df_af_183$grupp <- factor(df_af_183$grupp, levels = c("Dalarna\nkvinnor", "Dalarna\nmän",
                                                        "Gävleborg\nkvinnor", "Gävleborg\nmän",
                                                        "Värmland\nkvinnor", "Värmland\nmän",
                                                        "Norra Mellansverige\nkvinnor", "Norra Mellansverige\nmän", 
                                                        "Riket\nkvinnor", "Riket\nmän")) 
  
  df_af_183 <- df_af_183 %>% 
    mutate(bakgrundsvariabel = str_remove(bakgrundsvariabel, "ålder: "),
           utbildningsnivå = str_remove(utbildningsnivå, "utbildningsnivå: "),
           utb_grp = paste(bakgrundsvariabel, utbildningsnivå)) %>% 
    rename(andel = `Andel inskrivna arbetslösa, procent`)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("inskr_af_kon_alder_utbniva_df", df_af_183, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "diagram_183_inskr_af_utbniva.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df_af_183,
                               skickad_x_var = "grupp",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "utb_grp",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_y_axis_title = "procent",
                               x_axis_lutning = 0,
                               brew_palett = "Paired",
                               procent_0_100_10intervaller = TRUE,
                               #manual_color = diagramfarger("gron_sex")[c(3,4,5,6)],
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  
  
  
  return(gg_obj)
  
}
