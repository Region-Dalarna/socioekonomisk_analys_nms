skapa_andel_anstallda_bransch_diagram <- function(region_vekt = c("17", "20", "21"),
                                                  spara_diagrambildfil = FALSE,
                                                  returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar diagram för andel per branch (aggregerat). Enbart senaste år. BAS
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_syss_region_kon_sni2007_fodelseregion_prel_manad_ArbStDoNMNN_scb.R")
  options(dplyr.summarise.inform = FALSE)
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  nyckelmapp <- here("Indata/") %>% paste0(., "/")
  #nyckelmapp <- "G:/skript/nycklar/"
  branschnyckelfil <- "Bransch_Gxx_farger.csv"
  
  px_df <- hamta_bas_syss_region_kon_sni2007_fodelseregion_prel_manad_scb(region_vekt = region_vekt,
                                                                          kon_klartext = c("män","kvinnor"),
                                                                          cont_klartext = "sysselsatta efter arbetsställets belägenhet",
                                                                          tid_koder = "9999") %>%
    filter(`näringsgren SNI 2007`!="Total") %>% 
    separate(månad,into = c("år","månad"),sep="M") %>% 
    mutate(månad_namn = case_when(månad == "01" ~ "januari",
                                  månad == "02" ~ "februari",
                                  månad == "03" ~ "mars",
                                  månad == "04" ~ "april",
                                  månad == "05" ~ "maj",
                                  månad == "06" ~ "juni",
                                  månad == "07" ~ "juli",
                                  månad == "08" ~ "augusti",
                                  månad == "09" ~ "september",
                                  månad == "10" ~ "oktober",
                                  månad == "11" ~ "november",
                                  månad == "12" ~ "december"))

  branschnyckel <- read.csv2(paste0(nyckelmapp, branschnyckelfil),encoding = 'latin1')
  
  bransch_df <- left_join(px_df, branschnyckel %>% select(Br15kod, Bransch), by = c("sni2007kod" = "Br15kod"))
  
  bransch_aggr <- bransch_df %>% 
    group_by(år,månad_namn, kön, Bransch) %>% 
    summarise(syss = sum(`sysselsatta efter arbetsställets belägenhet`, na.rm = TRUE)) %>% 
    mutate(andel = round((syss / sum(syss))*100,1))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("syss_bransch_andel_aggr", bransch_aggr, envir = .GlobalEnv)
  }
  
  # =========================== Skapa diagram ==============================
  
  
  
  # Diagram med jämförelse mellan länen för senaste år
  diagramfil <- "diagram_27_syss_bransch_kon.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = bransch_aggr,
                             skickad_x_var = "Bransch",
                             skickad_y_var = "andel",
                             skickad_x_grupp = "kön",
                             manual_color = diagramfarger("kon"),
                             x_axis_lutning = 0,
                             diagram_liggande = TRUE,
                             x_axis_sort_value = TRUE,
                             manual_y_axis_title = "procent",
                             facet_legend_bottom = TRUE,
                             lagg_pa_logga = FALSE,
                             output_mapp = mapp,
                             filnamn_diagram = diagramfil,
                             skriv_till_diagramfil = spara_diagrambildfil)
  
  return(gg_obj)
  
}
