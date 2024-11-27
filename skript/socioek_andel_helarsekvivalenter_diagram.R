skapa_helarsekvivalenter_andel_lan <- function(region_vekt = c("17","20","21"),
                                        spara_diagrambildfil = FALSE,
                                        returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar tre diagram kopplade till låg ekonomisk standard
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_helarsekvivalenter_region_kon_aldersgrupp_tid_HE0000T02N2_scb.R")
  
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
  # ========================================== Inställningar ============================================
  
  mapp <- here("figurer/") %>% paste0(., "/")
  
  helarsekv <- hamta_helarsekvivalenter_region_kon_aldersgrupp_tid_scb(region_vekt = hamtakommuner(region_vekt,
                                                                                                   tamedlan = FALSE,
                                                                                                   tamedriket = TRUE),
                                                                       tid_koder = "9999",
                                                                       kon_klartext = "män och kvinnor totalt",
                                                                       aldersgrupp_klartext = "20-64 år")
  
  # Länen och riket
  # Summerar på länskod och beräknar andelar
  andel_ersattning_alla <- helarsekv %>% 
    filter(variabel %in% c("Summa helårsekvivalenter","Folkmängd")) %>% 
      mutate(lanskod = substr(regionkod,1,2)) %>% 
        group_by(lanskod,åldersgrupp,månad,variabel) %>%
          summarise(varde = sum(varde)) %>%
            pivot_wider(names_from = variabel, values_from = varde) %>% 
              mutate(Andel_ersattning = 100*(`Summa helårsekvivalenter`/Folkmängd),
                     region = skapa_kortnamn_lan(hamtaregion_kod_namn(lanskod)[[2]])) %>% 
                ungroup() %>% 
                  select(-lanskod)
  
  # Enbart NMS
  # Tar bort riket och summerar på totalen
  andel_ersattning_nms <- helarsekv %>%
    filter(variabel %in% c("Summa helårsekvivalenter","Folkmängd"),
           regionkod != "00") %>% 
      group_by(åldersgrupp,månad,variabel) %>%
        summarise(varde = sum(varde)) %>%
          pivot_wider(names_from = variabel, values_from = varde) %>% 
            mutate(Andel_ersattning = 100*(`Summa helårsekvivalenter`/Folkmängd),
                   region = "Norra Mellansverige")
  
  # Slår ihop de två datasetten
  andel_ersattning <- rbind(andel_ersattning_alla,andel_ersattning_nms) %>% 
    separate(månad, into = c("år","månad"), sep = "M") %>% 
      mutate(manad_namn = case_when(
        månad == "01" ~ "januari",
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
        månad == "12" ~ "december"
      ))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("ersattning_helarsakvivalenter_df", andel_ersattning, envir = .GlobalEnv)
  }
  
  andel_ersattning$region <- factor(andel_ersattning$region, 
                                               levels = c("Dalarna", "Gävleborg", "Värmland","Norra Mellansverige" ,"Riket")) 
  

  diagramfilnamn <- paste0("diagram_helarsekvivalenter_ersattning_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = andel_ersattning,
                               skickad_x_var = "region",
                               skickad_y_var = "Andel_ersattning",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               x_axis_lutning = 0,
                               manual_color =diagramfarger("gron_sex")[3],
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  return(gg_obj)
  
}
