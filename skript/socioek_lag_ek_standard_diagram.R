skapa_ekonomiskstandard_lan <- function(region_vekt = c("00","17","20","21"),
                                        spara_diagrambildfil = FALSE,
                                        diag_fargvekt = NA,
                                        returnera_dataframe_global_environment = TRUE
){
  
  # Skript som skapar tre diagram kopplade till låg ekonomisk standard
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_HE0110__HE0110F_scb.R", encoding = "utf-8", echo = FALSE)
  
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
  
  ekonomisk_standard_df <- hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_scb(region_vekt = region_vekt,
                                                                                                               alder_klartext = "*",
                                                                                                               inkomsttyp_klartext = "disponibel inkomst per k.e. inkl. kapitalvinst",
                                                                                                               cont_klartext = "Inkomst < 60 procent",			 
                                                                                                               tid_koder = "9999")	
  

  # Data till diagram 1
  ekonomisk_standard_20_64_df <- ekonomisk_standard_df %>% 
    filter(ålder == "20-64 år",
           `utländsk/svensk bakgrund` == "samtliga personer",
           sysselsättning %in% c("förvärvsarbetande","studerande","arbetslösa","sjuka","föräldralediga","övriga ej förvärvsarbetande")) %>% 
      mutate(region = skapa_kortnamn_lan(region),
             sysselsättning = paste0(toupper(substr(sysselsättning,1,1)),substr(sysselsättning,2,nchar(sysselsättning))))
  
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("lag_ek_standard_20_64_df", ekonomisk_standard_20_64_df, envir = .GlobalEnv)
  }

  ekonomisk_standard_20_64_df$region <- factor(ekonomisk_standard_20_64_df$region, 
                                    levels = rev(c("Dalarna", "Gävleborg", "Värmland", "Riket"))) 
  
  ekonomisk_standard_20_64_df$sysselsättning <- factor(ekonomisk_standard_20_64_df$sysselsättning, 
                                               levels = c("Förvärvsarbetande","Studerande","Arbetslösa","Sjuka","Föräldralediga","Övriga ej förvärvsarbetande")) 
  

  diagramfilnamn <- paste0("diagram_lagekstandard_sysselsattning_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ekonomisk_standard_20_64_df,
                               skickad_x_var = "sysselsättning",
                               skickad_y_var = "Inkomst < 60 procent",
                               skickad_x_grupp = "region",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_liggande = TRUE,
                               vand_sortering = TRUE,
                               legend_vand_ordning = TRUE,
                               x_axis_lutning = 0,
                               #manual_color =rev(diagramfarger("gron_sex")),
                               manual_color = diag_fargvekt,
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  # Data till diagram 2
  ekonomisk_standard_alder_df <- ekonomisk_standard_df %>% 
    filter(ålder %in% c("20-29 år","30-49 år","50-64 år","65-79 år","80- år"),
           `utländsk/svensk bakgrund` == "samtliga personer",
           sysselsättning == "samtliga personer") %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("lag_ek_standard_alder_df", ekonomisk_standard_alder_df, envir = .GlobalEnv)
  }
  
  ekonomisk_standard_alder_df$region <- factor(ekonomisk_standard_alder_df$region, 
                                               levels = c("Dalarna", "Gävleborg", "Värmland", "Riket"))
  
  diagramfilnamn <- paste0("diagram_lagekstandard_alder_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ekonomisk_standard_alder_df,
                               skickad_x_var = "region",
                               skickad_y_var = "Inkomst < 60 procent",
                               skickad_x_grupp = "ålder",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               #vand_sortering = TRUE,
                               #legend_vand_ordning = TRUE,
                               x_axis_lutning = 0,
                               #manual_color = diagramfarger("gron_sex")[c(1:4,6)],
                               manual_color = c(diag_fargvekt,"#666666"),
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  # Data till diagram 3
  ekonomisk_standard_bakgrund_df <- ekonomisk_standard_df %>% 
    filter(ålder == "20-64 år",
           `utländsk/svensk bakgrund` %in% c("utländsk bakgrund","svensk bakgrund"),
           sysselsättning %in% c("samtliga personer","förvärvsarbetande","icke förvärvsarbetande")) %>% 
    mutate(region = skapa_kortnamn_lan(region),
           sysselsättning = paste0(toupper(substr(sysselsättning,1,1)),substr(sysselsättning,2,nchar(sysselsättning))))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("lag_ek_standard_bakgrund_df", ekonomisk_standard_bakgrund_df, envir = .GlobalEnv)
  }
  
  ekonomisk_standard_bakgrund_df$region <- factor(ekonomisk_standard_bakgrund_df$region, 
                                               levels = c("Dalarna", "Gävleborg", "Värmland", "Riket"))
  
  ekonomisk_standard_bakgrund_df$sysselsättning <- factor(ekonomisk_standard_bakgrund_df$sysselsättning, 
                                                  levels = c("Samtliga personer","Förvärvsarbetande","Icke förvärvsarbetande"))
  
  diagramfilnamn <- paste0("diagram_lagekstandard_bakgrund_NMS_riket.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = ekonomisk_standard_bakgrund_df,
                               skickad_x_var = "sysselsättning",
                               skickad_y_var = "Inkomst < 60 procent",
                               skickad_x_grupp = "utländsk/svensk bakgrund",
                               output_mapp = mapp,
                               filnamn_diagram = diagramfilnamn,
                               diagram_facet = TRUE,
                               facet_grp = "region",
                               facet_scale = "fixed",
                               facet_legend_bottom = TRUE,
                               #vand_sortering = TRUE,
                               #legend_vand_ordning = TRUE,
                               x_axis_lutning = 0,
                               #manual_color = diagramfarger("gron_sex")[c(3,6)],
                               manual_color = diag_fargvekt,
                               manual_y_axis_title = "procent",
                               lagg_pa_logga = FALSE,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c("ek_standard_20_64", "ek_standard_alder", "ek_standard_bakgrund")
  
  return(gg_list)
  
}
