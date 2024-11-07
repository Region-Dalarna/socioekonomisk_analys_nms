
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

# ============= Övergång till högskola, folkhögskola och yrkeshögskola - motsvarar diagram 33 och 4 (sidor 17-18) i den tidigare rapporten
source(here("skript","socioek_övergång_eftergymn_studier_diagram.R"), encoding="UTF-8")
socioek_overgang_eftergymn_studier <- funktion_upprepa_forsok_om_fel( function() {
  skapa_overgang_eftergym_studier(region_vekt = c("00", "17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
overgang_eftergymn_studier_lasar <- overgang_eftergymn_studier_df$avgångsår %>% unique()
overgang_eftergymn_studier_avgangsar <- substr(overgang_eftergymn_studier_lasar, nchar(overgang_eftergymn_studier_lasar)-3, nchar(overgang_eftergymn_studier_lasar)) 

source(here("skript","socioek_övergång_eftergymn_tidsserie_diagram.R"), encoding="UTF-8")
socioek_overgang_eftergym_studier_tidsserie <- funktion_upprepa_forsok_om_fel( function() {
  skapa_overgang_eftergym_studier_tidsserie(region_vekt = c("00", "17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)



# ============= Sysselsättningsgrad uppdelat på län och kön - motsvarar diagram 57 (sidan 22) i den tidigare rapporten
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
sysselsattningsgrad_diagram <- funktion_upprepa_forsok_om_fel( function() {diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(tamedriket = TRUE),
                                                       kon_klartext = c("kvinnor","män"),
                                                       alder_klartext = "20-64 år",
                                                       ta_bort_diagramtitel = TRUE,
                                                       ta_bort_caption = TRUE,
                                                       valda_farger = diagramfarger("kon"),
                                                       diag_arbetskraftsdeltagande = FALSE,
                                                       diag_arbetslosthet = FALSE,
                                                       returnera_data = TRUE)}, hoppa_over = hoppa_over_felhantering)

sysselsattningsgrad_ar <- arbetsmarknadsstatus$ar %>% max() 
sysselsattningsgrad_alder <- arbetsmarknadsstatus$ålder %>% unique()
sysselsattningsgrad_manad <- arbetsmarknadsstatus$manad_long %>% unique() 

# ============= Andel inskrivna arbetslösa baserat på kön och ålder och utbildningsnivå - motsvarar diagram 21 (sidan 26) i den tidigare rapporten
source(here("skript","socioek_inskr_af_kon_alder_utbniva.R"), encoding="UTF-8")
inskr_af_kon_alder_utbniva <- funktion_upprepa_forsok_om_fel( function() {
  skapa_inskr_af_kon_alder_utbniva(region_vekt = c( "17", "20","21"))
}, hoppa_over = hoppa_over_felhantering)
inskr_arb_kon_alder_utbniva_ar <- inskr_af_kon_alder_utbniva_df$år %>% max()

# ============= Andel förvärvsarbetande uppdelat på kön och födelseregion - motsvarar diagram 23 (sidan 28) i den tidigare rapporten
source(here("skript","socioek_forvarvsarb_kon_fodelseregion.R"), encoding="UTF-8")
forvarvsarb_kon_fodelseregion <- funktion_upprepa_forsok_om_fel( function() {
  skapa_forvarsarb_kon_fodelseregion(region_vekt = c( "17", "20","21"))
}, hoppa_over = hoppa_over_felhantering)
forvarvsarbetande_kon_fodelseregion_ar <- forvarvsarbetande_kon_fodelseregion_df$år %>% max()

# ============= Andel förvärvsarbetande uppdelat på vistelsetid och utbildning - motsvarar diagram 24 (sidan 29) i den tidigare rapporten
source(here("skript","socioek_vistelsetid_utb_diagram.R"), encoding="UTF-8")
vistelsetid_utbildning_lan <- funktion_upprepa_forsok_om_fel( function() {
  skapa_vistelsetid_utbildning_lan(region_vekt = c( "17", "20","21"))
}, hoppa_over = hoppa_over_felhantering)
vistelsetid_utb_ar <- vistelsetid_utb_df$år %>% max()

# ============= Andel anställda i olika branscher uppdelat på kön (aggregerat över NMS) - motsvarar diagram 31 (sidan 36) i den tidigare rapporten
source(here("skript","socioek_andel_syss_bransch_diagram.R"), encoding="UTF-8")
andel_sysselsatta_kon_bransch <- funktion_upprepa_forsok_om_fel( function() {
  skapa_andel_anstallda_bransch_diagram(region_vekt = c("20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
sysselsatta_bransch_kon_ar <- syss_bransch_andel_aggr$år %>% max()
sysselsatta_bransch_kon_manad <- syss_bransch_andel_aggr$månad_namn %>% unique()

# # ============= Ohälsotal uppdelat på region och kön - motsvarar diagram 33 (sidan 39) i den tidigare rapporten
# Gammalt, baserat på Kolada
# source(here("skript","socioek_ohalsa_diagram.R"), encoding="UTF-8")
# ohalsa_diagram <- funktion_upprepa_forsok_om_fel( function() {
#   skapa_ohalsotal_diagram(region_vekt = c("00", "20", "17", "21"))
# }, hoppa_over = hoppa_over_felhantering)
# ohalsa_ar <- ohalsa_df$ar %>% max()

# ============= Ohälsotal uppdelat på region och kön - motsvarar diagram 33 (sidan 39) i den tidigare rapporten
# Uppdateras ej automatiskt utan kräver nedladdning av Excel-fil (se viktig info under Indata)
source(here("skript","socioek_ohalsa_diagram_korrekt.R"), encoding="UTF-8")
ohalsa_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_ohalsotal_lan()
}, hoppa_over = hoppa_over_felhantering)
ohalsa_ar <- ohalsotal_df$År %>% max()
ohalsa_manad <- ohalsotal_df$månad_namn %>% unique()

# ============= Sjukpenningtal uppdelat på region och kön och ålder (2 diagram) motsvarar diagram 34 och 35 (sidan 39-40) i den tidigare rapporten
# Uppdateras ej automatiskt utan kräver nedladdning av Excel-fil (se viktig info under Indata)
source(here("skript","socioek_sjukpenningtal_diagram.R"), encoding="UTF-8")
sjukpenningtal_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_sjukpenningtal_lan()
}, hoppa_over = hoppa_over_felhantering)
sjukpenningtal_ar <- sjukpenningtal_totalt_df$År %>% max()
sjukpenningtal_manad <- sjukpenningtal_totalt_df$månad_namn %>% unique()

# =============  Pågående sjukfall uppdelat på diagnoskapitel motsvarar diagram 36 (sidan 40) i den tidigare rapporten
# Uppdateras ej automatiskt utan kräver nedladdning av Excel-fil (se viktig info under Indata)
source(here("skript","socioek_pagaende_sjukfall_diagnos_diagram.R"), encoding="UTF-8")
pagaende_sjukfall_diagnos_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_pagaende_diagnos_lan()
}, hoppa_over = hoppa_over_felhantering)
pagaende_sjukfall_ar <- sjukfall_diagnos_df$År %>% max()
pagaende_sjukfall_manad <- sjukfall_diagnos_df$månad_namn %>% unique()

# ============= Låg ekonomisk standard uppdelat på sysselsatta, ålder och bakgrund - 3 diagram motsvarar diagram 40, 42 och 43 (sidor 45 - 48) i den tidigare rapporten
# Ej i markdown-fil ännu
source(here("skript","socioek_lag_ek_standard_diagram.R"), encoding="UTF-8")
lag_ek_standard_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_ekonomiskstandard_lan()
}, hoppa_over = hoppa_over_felhantering)
lag_ek_standard_20_64_ar <- lag_ek_standard_20_64_df$år %>% max()
lag_ek_standard_alder_ar <- lag_ek_standard_alder_df$år %>% max()
lag_ek_standard_bakgrund_ar <- lag_ek_standard_bakgrund_df$år %>% max()

# ============= Andel med ersättning av helårsekvivalenter - 3 diagram motsvarar diagram 41 (sidor 46) i den tidigare rapporten
# Ej i markdown-fil ännu
source(here("skript","socioek_andel_helarsekvivalenter_diagram.R"), encoding="UTF-8")
helarsekvivalenter_andel_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_helarsekvivalenter_andel_lan()
}, hoppa_over = hoppa_over_felhantering)
ersattning_helarsakvivalenter_ar <- ersattning_helarsakvivalenter_df$år %>% max()
ersattning_helarsakvivalenter_manad <- ersattning_helarsakvivalenter_df$manad_namn %>% unique()S

# ============= Innovationsindex på region - motsvarar diagram 46 (sidan 52) i den tidigare rapporten
# Uppdateras ej automatiskt utan kräver nedladdning av Excel-fil
source(here("skript","socioek_innovationsindex_diagram.R"), encoding="UTF-8")
innovationsindex_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_innovationsindex_diagram()
}, hoppa_over = hoppa_over_felhantering)
innovationsindex_ar <- innovationsindex_df$År %>% max()

# ============= Omsättning och antal arbetsställen (grupperat på NMS) - motsvarar diagram 47 och 48 (sidor 54-55) i den tidigare rapporten
source(here("skript","socioek_intakter_arbstallen_diagram.R"), encoding="UTF-8")
intakter_arbstallen_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_intakter_arbstallen_bransch_diagram(region_vekt = c("17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
intakter_bransch_ar <- intakter_bransch$år %>% max()
arbstallen_bransch_ar <- arbetsstallen_bransch$år %>% max()


# 2. om man vill knitta rapporten
#source(paste0(here("skript","/"), "2_knitta_rapport.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("skript","/"), "4_push_av_hela_repo_till_github.R"))


