
skapa_sjalvskattad_halsa_diagram <- function(
    returnera_data_rmarkdown = TRUE) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_sjalvskattad_halsa_region_halsotillstand_andel_och_konfidensintervall_kon_ar_hlv1allmxreg_fohm.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
  visa_dataetiketter <- FALSE
  gg_list <- list()
  
  hlv_df <- hamta_hlv_region_halsotillstand_andel_och_konfidensintervall_kon_ar_fohm(
  			region_vekt = c("20", "17", "21", "00"),			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
  			halsotillstand_klartext = "Bra eller mycket bra hälsa",			 #  Finns: "Bra eller mycket bra hälsa", "Dålig eller mycket dålig hälsa", "Långvarig sjukdom"
  			andel_och_konfidensintervall_klartext = "Andel",			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
  			kon_klartext = c("Kvinnor", "Män"),			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
  			tid_koder = "9999",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022"
  			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = "hlv.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  
  )
  
  if(returnera_data_rmarkdown == TRUE){
    assign("sjalvskattad_halsa_df", hlv_df, envir = .GlobalEnv)
  }
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(hlv_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(hlv_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(hlv_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(hlv_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  ar_txt <- if (min(hlv_df$År) == max(hlv_df$År)) min(hlv_df$År) else paste0(min(hlv_df$År), "_", max(hlv_df$År))
  halsa_txt <- unique(hlv_df$Hälsotillstånd) %>% tolower() %>% str_replace("hälsa", "självskattad hälsa")
  diagramtitel <- glue("Andel med {halsa_txt} år {ar_txt}")
  diagramfil <- glue("hlv_{regionfil_txt}_ar{ar_txt}.png") %>% str_replace_all("__", "_")

  chart_df <- hlv_df %>% 
    rename(andel = last_col()) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket"))
    )
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
  			 skickad_x_var = "region",
  			 skickad_y_var = "andel",
  			 skickad_x_grupp = "Kön",
  			 diagram_titel = NULL,
  			 x_axis_sort_value = FALSE,
  			 diagram_capt = diagram_capt,
  			 procent_0_100_10intervaller = TRUE,
  			 stodlinjer_avrunda_fem = TRUE,
  			 filnamn_diagram = diagramfil,
  			 x_axis_lutning = 0,
  			 manual_y_axis_title = "procent",
  			 manual_color = diagramfarger("kon"),
  			 output_mapp = output_mapp
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
} # slut funktion