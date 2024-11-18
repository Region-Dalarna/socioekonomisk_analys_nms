
skapa_af_diagram_lista <- function(
    region_vekt = "20",
    logga_path = NA,
    logga_i_diagram = FALSE,
    ta_bort_diagram_titel = FALSE,
    skriv_diagramfil = FALSE,
    diag_fargvekt = NA,
    diag_fargvekt_kon = NA,
    output_mapp = NA,
    returnera_dataframe_global_environment = TRUE
  ) {

  library(tidyverse)
  #library(openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # hantering av output mapp
  if (all(is.na(output_mapp)) & skriv_diagramfil) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde om du vill skriva en diagrambildfil.")
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("bla_gra_fyra")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt_kon))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt_kon <- diagramfarger("kon")
    } else {
      diag_fargvekt_kon <- hue_pal()(9)
    }
  }
  
  gg_list <- list()
  
  # ============================= hämta alla arbetslöshetsdata ====================================
  
  source("G:/skript/peter/hamta_inskrivna_arbetslosa_tid_utan_arbete_af.R")
  source("G:/skript/peter/hamta_arbetskraft_bas_af.R")
  
  alla_arblos_af <- hamta_inskrivna_arbetslosa_tid_utan_arbete_af(region_vekt = c("20", "21", "17", "00"),
                                                                  dataset = "*")
  
  alla_arbkraft_af <- hamta_arbetskraft_bas_af(region_vekt = c("20", "21", "17", "00"),
                                               dataset = "*")
  
  arblos_kon <- alla_arblos_af$Kön
  tid_vekt <- period_jmfr_filter(period_kolumn = arblos_kon$Tid, max(arblos_kon$Tid), period_vekt = c(-12, -24, -36))
  arblos_kon <- arblos_kon %>% 
    filter(Tid %in% tid_vekt)
  
  arbkraft_kon <- alla_arbkraft_af$Kön
  tid_vekt <- period_jmfr_filter(period_kolumn = arbkraft_kon$Tid, max(arbkraft_kon$Tid), period_vekt = c(-12, -24, -36))
  
  arbkraft_kon <- arbkraft_kon %>% 
    filter(Tid %in% tid_vekt) %>% 
    mutate(Kön = Kön %>% tolower())
  
  # ============================= diagram arbetslösa utifrån arbetslöshetstid ====================================
  
  af_dia14 <- arblos_kon %>% 
    left_join(arbkraft_kon, by = c("Tid", "År", "Månad", "År_månad", "Månad_år", "Regionkod", "Region", "Kön"))
  
  af_dia14_2 <- af_dia14 %>%
    mutate(Kön = tolower(Kön),
           Region = Region %>% skapa_kortnamn_lan(F)) %>% 
     mutate(`totalt månader` = Arbetslösa / Antal,
           `< 6 månader` = ((Arbetslösa - `Utan arbete mer än 6 månader`)) / Antal,
           `6-12 månader` = (`Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`) / Antal,
           `12-24 månader` = (`Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`) / Antal,
           `> 24 månader` = (`Utan arbete mer än 24 månader`) / Antal) %>% 
    select(-c(contains("Utan arbete mer än"))) %>% 
    pivot_longer(cols = contains("månader"), names_to = "Längd", values_to = "Andel") %>% 
    mutate(Längd = ifelse(Längd == "totalt månader", "totalt", Längd), 
           Längd = factor(Längd, levels = c("totalt", "< 6 månader", "6-12 månader", "12-24 månader", 
                                            "> 24 månader")),
           Region = factor(Region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")),
           proc = Andel * 100) %>% 
    filter(Tid == max(Tid))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("arblosa_kon_arbloshetstid", af_dia14_2, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "andel_arblosa_arbloshetstid.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = af_dia14_2 %>% 
                       filter(Längd != "totalt"),
                     skickad_x_var = "Region",
                     skickad_y_var = "proc",
                     skickad_x_grupp = "Kön",
                     output_mapp = output_mapp,
                     filnamn_diagram = diagramfilnamn,
                     stodlinjer_avrunda_fem = TRUE,
                     # manual_x_axis_text_vjust = 1,
                     # manual_x_axis_text_hjust = 1,
                     x_axis_lutning = 0,
                     geom_position_stack = FALSE,
                     manual_y_axis_title = "procent",
                     manual_color = diag_fargvekt_kon,
                     skriv_till_diagramfil = skriv_diagramfil,
                     utan_diagramtitel = ta_bort_diagram_titel,
                     diagram_facet = TRUE,
                     facet_grp = "Längd",
                     facet_scale = "fixed",
                     facet_legend_bottom = TRUE,
                     legend_vand_ordning = TRUE,
                     lagg_pa_logga = logga_i_diagram,
                     logga_path = logga_path
                     )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  # ============================= Andel arbetslösa över tid per län ====================================
  
  #source("G:/skript/peter/hamta_inskrivna_arbetslosa_tid_utan_arbete_af.R")
  af_arblos_tid <- alla_arblos_af$Total
  
  
  #source("G:/skript/peter/hamta_arbetskraft_bas_af.R")
  af_arbkraft_tid <- alla_arbkraft_af$Total
  
  af_arblos_tid_2 <- af_arblos_tid %>% 
    filter(Tid %in% unique(af_arbkraft_tid$Tid))
  
  arblos_andel_df <- af_arblos_tid_2 %>% 
    left_join(af_arbkraft_tid, by = c("Tid", "År", "Månad", "År_månad", "Månad_år", "Regionkod", "Region"))
  
  arblos_andel_df <- arblos_andel_df %>%
    mutate(Region = Region %>% skapa_kortnamn_lan(F)) %>% 
    mutate(`totalt månader` = Arbetslösa / Antal,
           `< 6 månader` = ((Arbetslösa - `Utan arbete mer än 6 månader`)) / Antal,
           `6-12 månader` = (`Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`) / Antal,
           `12-24 månader` = (`Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`) / Antal,
           `> 24 månader` = (`Utan arbete mer än 24 månader`) / Antal) %>% 
    select(-c(contains("Utan arbete mer än"))) %>% 
    pivot_longer(cols = contains("månader"), names_to = "Längd", values_to = "Andel") %>% 
    mutate(Längd = ifelse(Längd == "totalt månader", "totalt", Längd), 
           Längd = factor(Längd, levels = c("totalt", "< 6 månader", "6-12 månader", "12-24 månader", 
                                            "> 24 månader") %>% rev()),
           proc = Andel * 100) %>% 
    mutate(Region = factor(Region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("arblosa_over_tid", arblos_andel_df, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "linjediagram_andel_arblosa_over_tid.png"
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = arblos_andel_df %>% 
                      filter(Längd == "totalt"),
                    skickad_x_var = "Månad_år",
                    skickad_y_var = "proc",
                    skickad_x_grupp = "Region",
                    output_mapp = output_mapp,
                    filnamn_diagram = diagramfilnamn,
                    stodlinjer_avrunda_fem = TRUE,
                    manual_y_axis_title = "procent",
                    manual_color = diag_fargvekt,
                    facet_legend_bottom = TRUE,
                    skriv_till_diagramfil = skriv_diagramfil,
                    utan_diagramtitel = ta_bort_diagram_titel,
                    lagg_pa_logga = logga_i_diagram,
                    logga_path = logga_path
                    )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  
  
  # ============================= Andel arbetslösa unga över tid per län ====================================
  
  af_arblos_df <- alla_arblos_af$Ålder %>% 
    filter(Ålder == "18-24") %>% 
    select(-Ålder)
  
  af_arbkraft_df <- alla_arbkraft_af$Ålder %>% 
    filter(Ålder == "18-24 år")
    
  
  af_arblos_df<- af_arblos_df %>% 
    filter(Tid %in% unique(af_arbkraft_df$Tid))
  
  arblos_andel_df <- af_arblos_df %>% 
    left_join(af_arbkraft_df, by = c("Tid", "År", "Månad", "År_månad", "Månad_år", "Regionkod", "Region"))
  
  arblos_andel_df <- arblos_andel_df %>%
    mutate(Region = Region %>% skapa_kortnamn_lan(F)) %>% 
    mutate(`totalt månader` = Arbetslösa / Antal,
           `< 6 månader` = ((Arbetslösa - `Utan arbete mer än 6 månader`)) / Antal,
           `6-12 månader` = (`Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`) / Antal,
           `12-24 månader` = (`Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`) / Antal,
           `> 24 månader` = (`Utan arbete mer än 24 månader`) / Antal) %>% 
    select(-c(contains("Utan arbete mer än"))) %>% 
    pivot_longer(cols = contains("månader"), names_to = "Längd", values_to = "Andel") %>% 
    mutate(Längd = ifelse(Längd == "totalt månader", "totalt", Längd), 
           Längd = factor(Längd, levels = c("totalt", "< 6 månader", "6-12 månader", "12-24 månader", 
                                            "> 24 månader") %>% rev()),
           proc = Andel * 100) %>% 
    mutate(Region = factor(Region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("arblosa_unga_over_tid", arblos_andel_df, envir = .GlobalEnv)
  }
  
  diagramfilnamn <- "linjediagram_andel_unga_arblosa_over_tid.png"
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = arblos_andel_df %>% 
                                filter(Längd == "totalt"),
                              skickad_x_var = "Månad_år",
                              skickad_y_var = "proc",
                              skickad_x_grupp = "Region",
                              output_mapp = output_mapp,
                              filnamn_diagram = diagramfilnamn,
                              stodlinjer_avrunda_fem = TRUE,
                              manual_y_axis_title = "procent",
                              manual_color = diag_fargvekt,
                              facet_legend_bottom = TRUE,
                              skriv_till_diagramfil = skriv_diagramfil,
                              utan_diagramtitel = ta_bort_diagram_titel,
                              lagg_pa_logga = logga_i_diagram,
                              logga_path = logga_path
                              )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
 
# ============================= Andel arbetslösa utrikes födda över tid per län ====================================

af_arblos_utr_df <- alla_arblos_af$Födelseland %>% 
  filter(Födelseland != "Sverige") %>% 
  group_by(Tid, År, Månad, År_månad, Månad_år, Regionkod, Region) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

af_arbkraft_utr_df <- alla_arbkraft_af$Födelseland %>% 
  filter(Födelseland == "utrikes")

af_arblos_utr_df<- af_arblos_utr_df %>% 
  filter(Tid %in% unique(af_arbkraft_utr_df$Tid))

arblos_andel_utr_df <- af_arblos_utr_df %>% 
  left_join(af_arbkraft_utr_df, by = c("Tid", "År", "Månad", "År_månad", "Månad_år", "Regionkod", "Region"))

arblos_andel_utr_df <- arblos_andel_utr_df %>%
  mutate(Region = Region %>% skapa_kortnamn_lan(F)) %>% 
  mutate(`totalt månader` = Arbetslösa / Antal,
         `< 6 månader` = ((Arbetslösa - `Utan arbete mer än 6 månader`)) / Antal,
         `6-12 månader` = (`Utan arbete mer än 6 månader` - `Utan arbete mer än 12 månader`) / Antal,
         `12-24 månader` = (`Utan arbete mer än 12 månader` - `Utan arbete mer än 24 månader`) / Antal,
         `> 24 månader` = (`Utan arbete mer än 24 månader`) / Antal) %>% 
  select(-c(contains("Utan arbete mer än"))) %>% 
  pivot_longer(cols = contains("månader"), names_to = "Längd", values_to = "Andel") %>% 
  mutate(Längd = ifelse(Längd == "totalt månader", "totalt", Längd), 
         Längd = factor(Längd, levels = c("totalt", "< 6 månader", "6-12 månader", "12-24 månader", 
                                          "> 24 månader") %>% rev()),
         proc = Andel * 100) %>% 
  mutate(Region = factor(Region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")))

if(returnera_dataframe_global_environment == TRUE){
  assign("arblosa_utr_over_tid", arblos_andel_utr_df, envir = .GlobalEnv)
}

diagramfilnamn <- "linjediagram_andel_utr_arblosa_over_tid.png"

gg_obj <- SkapaLinjeDiagram(skickad_df = arblos_andel_utr_df %>% 
                              filter(Längd == "totalt"),
                            skickad_x_var = "Månad_år",
                            skickad_y_var = "proc",
                            skickad_x_grupp = "Region",
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfilnamn,
                            stodlinjer_avrunda_fem = TRUE,
                            manual_y_axis_title = "procent",
                            manual_color = diag_fargvekt,
                            facet_legend_bottom = TRUE,
                            skriv_till_diagramfil = skriv_diagramfil,
                            utan_diagramtitel = ta_bort_diagram_titel,
                            lagg_pa_logga = logga_i_diagram,
                            logga_path = logga_path
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

return(gg_list) 

} # slut diagram-funktion

