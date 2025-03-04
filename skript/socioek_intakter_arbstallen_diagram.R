skapa_intakter_arbstallen_bransch_diagram <- function(region_vekt = c("17", "20", "21"),
                                                  spara_diagrambildfil = FALSE,
                                                  diag_fargvekt = NA,
                                                  returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar diagram för andel per branch (aggregerat). Enbart senaste år. BAS
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
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
  nyckelmapp <- here("Indata/") %>% paste0(., "/")
  #nyckelmapp <- "G:/skript/nycklar/"
  nyckel_fil <- paste0(nyckelmapp, "Bransch_FEK.xlsx")
  
  #nyckel_df <- read_xlsx(nyckel_fil)
  nyckel_df <- readxl::read_xlsx(nyckel_fil)
  
  px_df <- NULL
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV0109/NV0109L/RegionalBasf07"
  
  #url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV0109/NV0109P/NSEBasfaktaLVEngs07"
  
  # hämta senaste år
  sen_ar <- max(hamta_giltiga_varden_fran_tabell(url_uttag, "tid"))
  
  varlista <- list(
    Region = region_vekt,
    SNI2007 = '*',
    ContentsCode ='*',
    Tid = sen_ar
  )
  
  
  # API-uttaget
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region, SNI2007))  
  
  px_df <- px_df %>% 
    rename(regionkod = Region, branschkod = SNI2007) %>% 
    relocate(regionkod, .before = region) %>% 
    relocate(branschkod, .before = `näringsgren SNI 2007`) %>% 
    left_join(nyckel_df %>% select(Kod, Branschgrupp), by = c("branschkod" = "Kod"))
  
  varde_df <- px_df %>% 
    group_by(år,Branschgrupp) %>% 
    summarise(intakter = sum(`Totala Intäkter, mnkr`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(intakter > 0)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("intakter_bransch", varde_df, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "diagram_46_intakter.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = varde_df,
                               skickad_x_var = "Branschgrupp",
                               skickad_y_var = "intakter",
                               skickad_x_grupp = NA,
                               x_axis_sort_value = TRUE,
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "totala intäkter, mkr",
                               x_axis_lutning = 45,
                               #brew_palett = "Paired",
                               #manual_color = rev(diagramfarger("gron_sex")[c(3,5)]),
                               manual_color = diag_fargvekt,
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  arbst_df <- px_df %>% 
    group_by(år,Branschgrupp) %>% 
    summarise(arbst = sum(`Antal arbetsställen`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(arbst > 0)
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("arbetsstallen_bransch", arbst_df, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "diagram_47_arbetsstallen.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbst_df,
                               skickad_x_var = "Branschgrupp",
                               skickad_y_var = "arbst",
                               skickad_x_grupp = NA,
                               x_axis_sort_value = TRUE,
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "antal arbetsställen",
                               x_axis_lutning = 45,
                               #brew_palett = "Paired",
                               #manual_color = rev(diagramfarger("gron_sex")[c(3,5)]),
                               manual_color = diag_fargvekt,
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c("intakter", "arbetsstallen")
  
  return(gg_list)
  
}
