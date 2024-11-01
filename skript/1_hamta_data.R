
if (!require("pacman")) install.packages("pacman")
p_load(here)

# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
hoppa_over_felhantering = TRUE


source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# tabell över befolkningsutvecklingen i NMS-länen samt riket per åldersgrupp
source(here("skript","socioek_beftabell.R"), encoding="UTF-8")
beftabell_flextable <- funktion_upprepa_forsok_om_fel( function() {
  skapa_bef_tabell()
}, hoppa_over = hoppa_over_felhantering)

# diagram över befolkningsutvecklingen
source(here("skript","socioek_befutv_diagram.R"), encoding="UTF-8")
befutv_linjediagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_befutv_diagram(region_vekt = c("17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
bef_utv_slutar <- befutv_linjediagram$data$år %>% max()


# diagram över befolkningsförändringar per län i NMS
source(here("skript","socioek_befforandr_diagram.R"), encoding="UTF-8")
befforandringar_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_beffor_diagram(region_vekt = c("17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
beffor_ar <- beffor_df$år %>% max()


# diagram över invandring per län i NMS från år 2000 och framåt - linjediagram
source(here("skript","socioek_utrinflyttning_diagram.R"), encoding="UTF-8")
inv_nms_linjediagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_invandring_diagram(region_vekt = c("17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
inv_ar <- inv_nms_linjediagram$data$år %>% max

# ============= diagram befolkningsprognoser 
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_befolkningsprognos_scb_api_profet.R")

# diagram över befolkningsprognoser för NMS-länen sammantaget
befprogn_diagram_nms <- funktion_upprepa_forsok_om_fel( function() {
  SkapaBefPrognosDiagram(region_vekt = c("17", "20", "21"),
                         gruppera_namn = "Norra Mellansverige",
                         utan_diagramtitel = TRUE,
                         ta_med_logga = FALSE,
                         spara_dataframe_till_global_environment = TRUE,
                         tabeller_url = c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                          "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),
                         skapa_fil = FALSE)
}, hoppa_over = hoppa_over_felhantering)


# diagram över befolkningsprognoser för NMS-länen var och ett för sig
befprogn_diagram_lan <- funktion_upprepa_forsok_om_fel( function() {
  SkapaBefPrognosDiagram(region_vekt = c("17", "20", "21"),
                         utan_diagramtitel = TRUE,
                         ta_med_logga = FALSE,
                         tabeller_url = c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                          "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),
                         facet_variabel = "region",
                         facet_scale = "fixed",
                         skapa_fil = FALSE)
}, hoppa_over = hoppa_over_felhantering)
  
# extrahera start- och slutår
befprogn_startar <- bef_progn_nms_df$start_ar %>% unique()
befprogn_slutar <- bef_progn_nms_df$slut_ar %>% unique()

# skapa textsträng som blir korrekt oavsett antalet jämförelseår
if (length(befprogn_startar) == 1) {
  befprogn_arsintervall <- paste(befprogn_startar[1], "-", befprogn_slutar[1], sep = "")
} else {
  # Skapa listan över intervaller
  intervaller <- paste(befprogn_startar, "-", befprogn_slutar, sep = "")
  # Sätt ihop textsträngen med ", " och " respektive " på slutet
  befprogn_arsintervall <- paste(paste(intervaller[-length(intervaller)], collapse = ", "), "respektive", intervaller[length(intervaller)])
}

# ============= diagram utbildningsnivåer eftergymnasialt
source(here("skript","socioek_andel_eftergymn_utb_diagram.R"), encoding="UTF-8")
utbniva_eftergymn_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_utb_eftergymn_kon_diagram(region_vekt = c("00", "17", "20", "21"))
  }, hoppa_over = hoppa_over_felhantering)
utb_niva_eftergymn_ar <- utb_niva_eftergymn_kon_df$år %>% max()


# ============= diagram utbildningsnivåer högutbildade
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")

utb_niva_hogutb_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("00", "20", "17", "21"),
                                     region_lagg_forst = c("20", "21", "17"),
                                     sverige_istallet_for_riket = FALSE,
                                     diag_hogutb_over_tid = TRUE,
                                     facet_x_axis_stlk = 6,
                                     diag_lagutb_over_tid = FALSE,
                                     diag_andel_alla_utbnivaer = FALSE,
                                     diag_andel_utbniva_jmfr_lan = FALSE)
  }, hoppa_over = hoppa_over_felhantering)
  
# ============= diagram andel eftergymnasialt utbildade efter födelseregion och kön
source(here("skript","socioek_eftergymn_utb_kon_fodelseregion.R"), encoding="UTF-8")

utb_niva_eftergymn_bakgr_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_utbniva_bakgrund_kon_diagram(region_vekt = c("00", "20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
utbniva_eftergymn_bakgr_ar <- utbniva_eftergymn_bakgr_kon_df$år %>% max()

# ============= diagram andel gymnasiebehöriga efter födelseregion och kön
source(here("skript","socioek_gymn_behorighet_kon_inr_utr_fodda.R"), encoding="UTF-8")
utb_niva_eftergymn_bakgr_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_gymn_behorighet_inr_utr_fodda_kon_diagram(region_vekt = c("00", "20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
gymn_behorighet_ar <- gymn_behorighet_bakgr_kon_df$år %>% max()

# 2. om man vill knitta rapporten
#source(paste0(here("skript","/"), "2_knitta_rapport.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("skript","/"), "4_push_av_hela_repo_till_github.R"))


