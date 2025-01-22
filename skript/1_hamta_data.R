
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here)

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

# ============= diagram utbildningsnivåer eftergymnasialt - Justerat felaktighet (döpt om Antal till Befolkning)
source(here("skript","socioek_andel_eftergymn_utb_diagram.R"), encoding="UTF-8")
utbniva_eftergymn_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_utb_eftergymn_kon_diagram(region_vekt = c("00", "17", "20", "21"))
  }, hoppa_over = hoppa_over_felhantering)
utb_niva_eftergymn_ar <- utb_niva_eftergymn_kon_df$år %>% max()


# ============= diagram utbildningsnivåer högutbildade - - Justerat felaktighet (döpt om Antal till Befolkning)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")

utb_niva_hogutb_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("00", "20", "17", "21"),
                                     region_lagg_forst = c("20", "21", "17"),
                                     sverige_istallet_for_riket = FALSE,
                                     diag_hogutb_over_tid = TRUE,
                                     facet_x_axis_stlk = 6,
                                     visa_var_xe_etikett = 2,
                                     diagramtitel_tabort = TRUE,
                                     diag_lagutb_over_tid = FALSE,
                                     diag_andel_alla_utbnivaer = FALSE,
                                     diag_andel_utbniva_jmfr_lan = FALSE)
  }, hoppa_over = hoppa_over_felhantering) %>% .[[1]]
  
# ============= diagram andel eftergymnasialt utbildade efter födelseregion och kön
source(here("skript","socioek_eftergymn_utb_kon_fodelseregion.R"), encoding="UTF-8")

utb_niva_eftergymn_bakgr_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_utbniva_bakgrund_kon_diagram(region_vekt = c("00", "20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
utbniva_eftergymn_bakgr_ar <- utbniva_eftergymn_bakgr_kon_df$år %>% max()

# ============= diagram andel gymnasiebehöriga efter födelseregion och kön
source(here("skript","socioek_gymn_behorighet_kon_inr_utr_fodda.R"), encoding="UTF-8")
gymn_behorighet_kon_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_gymn_behorighet_inr_utr_fodda_kon_diagram(region_vekt = c("00", "20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
gymn_behorighet_ar <- gymn_behorighet_bakgr_kon_df$år %>% max()

# ============= Övergång till högskola, folkhögskola och yrkeshögskola - motsvarar diagram 33 och 4 (sidor 17-18) i den tidigare rapporten
# Tillfälligt borttaget då SCB gjort ändringar i API:et
source(here("skript","socioek_övergång_eftergymn_studier_diagram.R"), encoding="UTF-8")
socioek_overgang_eftergymn_studier <- funktion_upprepa_forsok_om_fel( function() {
  skapa_overgang_eftergym_studier(region_vekt = c("00", "17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
overgang_eftergymn_studier_lasar <- overgang_eftergymn_studier_df$avgångsår %>% unique()
overgang_eftergymn_studier_avgangsar <- substr(overgang_eftergymn_studier_lasar, nchar(overgang_eftergymn_studier_lasar)-3, nchar(overgang_eftergymn_studier_lasar))

overgang_eftergymn_studier_riket_varde_kvinnor <- gsub("\\.",",",sum(overgang_eftergymn_studier_df %>% filter(region == "Riket",kön == "kvinnor") %>% .$ andel))
overgang_eftergymn_studier_riket_varde_män <- gsub("\\.",",",sum(overgang_eftergymn_studier_df %>% filter(region == "Riket",kön == "män") %>% .$ andel))

overgang_eftergymn_studier_NMS_varde_kvinnor <- gsub("\\.",",",round((sum(overgang_eftergymn_studier_df %>% filter(region != "Riket",kön == "kvinnor") %>% .$`Antal som läst vidare inom 3 år`)/sum(overgang_eftergymn_studier_df %>% filter(region != "Riket",kön == "kvinnor",utbtyp == "Högskola") %>% .$`Antal gymnasieexaminerade totalt`))*100,1))
overgang_eftergymn_studier_NMS_varde_man <- gsub("\\.",",",round((sum(overgang_eftergymn_studier_df %>% filter(region != "Riket",kön == "män") %>% .$`Antal som läst vidare inom 3 år`)/sum(overgang_eftergymn_studier_df %>% filter(region != "Riket",kön == "män",utbtyp == "Högskola") %>% .$`Antal gymnasieexaminerade totalt`))*100,1))

#inte klart - Mats och Peter
overgang_nms <- overgang_eftergymn_studier_df %>% 
  group_by(avgångsår, gymnasieprogram, kön, utbtyp) %>% 
  summarise(antal_3ar = sum(`Antal som läst vidare inom 3 år`, na.rm = TRUE), 
            antal_tot = sum(`Antal gymnasieexaminerade totalt`, na.rm = TRUE)) %>% 
  mutate(andel = antal_3ar/antal_tot) %>% 
  ungroup()

overgang_nms <- overgang_eftergymn_studier_df %>% 
  group_by(avgångsår, gymnasieprogram, kön) %>% 
  summarise(antal_3ar = sum(`Antal som läst vidare inom 3 år`, na.rm = TRUE), 
            antal_tot = sum(`Antal gymnasieexaminerade totalt`, na.rm = TRUE)) %>% 
  mutate(andel = antal_3ar/antal_tot) %>% 
  ungroup()


source(here("skript","socioek_övergång_eftergymn_tidsserie_diagram.R"), encoding="UTF-8")
socioek_overgang_eftergym_studier_tidsserie <- funktion_upprepa_forsok_om_fel( function() {
  skapa_overgang_eftergym_studier_tidsserie(region_vekt = c("00", "17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)

# ============= UVAS - 3 diagram. Motsvarar sidor 18-20 i den gamla rapporten (diagram 15-16)
# OBS - uppdateras inte automatiskt utan kräver nedladdning av Excel-fil (se viktig info under Indata)
source(here("skript","socioek_UVAS_diagram.R"), encoding="UTF-8")
UVAS <- funktion_upprepa_forsok_om_fel( function() {
  skapa_UVAS_diagram(spara_diagrambildfil = FALSE)
}, hoppa_over = hoppa_over_felhantering)
UVAS_ar <- UVAS_df$År %>% max()

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

# År, månad och åldersgrupp
sysselsattningsgrad_ar <- arbetsmarknadsstatus$ar %>% max() 
sysselsattningsgrad_alder <- arbetsmarknadsstatus$ålder %>% unique()
sysselsattningsgrad_manad <- arbetsmarknadsstatus$manad_long %>% unique()

# Lägsta sysselsättningsgrad NMS
sysselsattningsgrad_man_min_varde <- gsub("\\.",",",min(arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "män") %>% .$varde))
sysselsattningsgrad_man_min_region <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "män") %>% filter(varde==min(varde)) %>% .$region)
sysselsattningsgrad_kvinnor_min_varde <- gsub("\\.",",",min(arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "kvinnor") %>% .$varde))
sysselsattningsgrad_kvinnor_min_region <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "kvinnor") %>% filter(varde==min(varde)) %>% .$region)

# Högsta sysselsättningsgrad NMS
sysselsattningsgrad_man_max_varde <- gsub("\\.",",",max(arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "män") %>% .$varde))
sysselsattningsgrad_man_max_region <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "män") %>% filter(varde==max(varde)) %>% .$region)
sysselsattningsgrad_kvinnor_max_varde <- gsub("\\.",",",max(arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "kvinnor") %>% .$varde))
sysselsattningsgrad_kvinnor_max_region <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"),kön == "kvinnor") %>% filter(varde==max(varde)) %>% .$region)

# Riket
sysselsattningsgrad_man_riket <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region == "Sverige",kön == "män") %>% .$varde)
sysselsattningsgrad_kvinnor_riket <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region == "Sverige",kön == "kvinnor") %>% .$varde)

# ============= Andel inskrivna arbetslösa baserat på kön och ålder och utbildningsnivå - motsvarar diagram 21 (sidan 26) i den tidigare rapporten - Har ändrat med ny länk för SCB-data
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
forvarvsarbetande_kon_fodelseregion_skillnad_gavleborg <- gsub("\\.",",",round(forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Gävleborgs län", kön == "män",bakgrundsvariabel == "övriga världen") %>% .$andel - forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Gävleborgs län", kön == "kvinnor",bakgrundsvariabel == "övriga världen") %>% .$andel,1))
forvarvsarbetande_kon_fodelseregion_skillnad_varmland <- gsub("\\.",",",round(forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Värmlands län", kön == "män",bakgrundsvariabel == "övriga världen") %>% .$andel - forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Värmlands län", kön == "kvinnor",bakgrundsvariabel == "övriga världen") %>% .$andel,1))
forvarvsarbetande_kon_fodelseregion_skillnad_dalarna <- gsub("\\.",",",round(forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Dalarnas län", kön == "män",bakgrundsvariabel == "övriga världen") %>% .$andel - forvarvsarbetande_kon_fodelseregion_df %>% filter(region == "Dalarnas län", kön == "kvinnor",bakgrundsvariabel == "övriga världen") %>% .$andel,1))

# Nedan hämtas data för sysselsättningsgrad för utrikes födda. Detta för att ersätta hårdkodad data under diagram 23 i den nya rapporten.
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_ny_BAS_scb.R")
sysselsatta_utrikes_andel <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny(region_vekt = c("17", "20", "21"),
                                                                                                    kon_klartext = "män och kvinnor",
                                                                                                    utbniv_klartext = "*",
                                                                                                    bakgrvar_klartext = c("födelseregion: Sverige","samtliga utrikes födda"),
                                                                                                    cont_klartext = "Andel sysselsatta",
                                                                                                    tid_koder = "9999")

syss_ar <- sysselsatta_utrikes_andel$år %>% max()

syss_utrikes_andel_eftergym_min <- round(sysselsatta_utrikes_andel %>% filter(utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning",bakgrundsvariabel == "samtliga utrikes födda") %>% .$`Andel sysselsatta` %>% min(),0)
syss_utrikes_andel_eftergym_max <- round(sysselsatta_utrikes_andel %>% filter(utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning",bakgrundsvariabel == "samtliga utrikes födda") %>% .$`Andel sysselsatta` %>% max(),0)

syss_inrikes_andel_eftergym_min <- round(sysselsatta_utrikes_andel %>% filter(utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning",bakgrundsvariabel == "födelseregion: Sverige") %>% .$`Andel sysselsatta` %>% min(),0)
syss_inrikes_andel_eftergym_max <- round(sysselsatta_utrikes_andel %>% filter(utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning",bakgrundsvariabel == "födelseregion: Sverige") %>% .$`Andel sysselsatta` %>% max(),0)

# ============= Andel förvärvsarbetande uppdelat på vistelsetid och utbildning - motsvarar diagram 24 (sidan 29) i den tidigare rapporten
source(here("skript","socioek_vistelsetid_utb_diagram.R"), encoding="UTF-8")
vistelsetid_utbildning_lan <- funktion_upprepa_forsok_om_fel( function() {
  skapa_vistelsetid_utbildning_lan(region_vekt = c( "17", "20","21"))
}, hoppa_over = hoppa_over_felhantering)
vistelsetid_utb_ar <- vistelsetid_utb_df$år %>% max()

# Nedan hämtas data för sysselsättningsgrad uppdelat vistelsetid. Detta för att ersätta hårdkodad data under diagram 23 i den nya rapporten.
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_ny_BAS_scb.R")
sysselsatta_vistelsetid_andel <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny(region_vekt = c("17", "20", "21"),
                                                                                           kon_klartext = c("män","kvinnor"),
                                                                                           utbniv_klartext = "*",
                                                                                           bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                                           cont_klartext = "Andel sysselsatta",			 #  Finns: "Andel sysselsatta", "Andel företagare av de sysselsatta", "Andel inskrivna arbetslösa", "Andel öppet arbetslösa", "Andel sökande i program med aktivitetsstöd", "Andel långtidsarbetslösa", "Andel fortfarande sysselsatta efter ett år", "Andel fortfarande företagare efter ett år", "Andel i chefsposition", "Andel ,
                                                                                           tid_koder = "9999")


syss_0_1_kvinnor_ar_forgym_min <- round(sysselsatta_vistelsetid_andel %>% filter(kön == "kvinnor",bakgrundsvariabel == "vistelsetid 0-1 år",utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$`Andel sysselsatta` %>% min(),0)
syss_0_1_kvinnor_ar_forgym_max <- round(sysselsatta_vistelsetid_andel %>% filter(kön == "kvinnor", bakgrundsvariabel == "vistelsetid 0-1 år",utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$`Andel sysselsatta` %>% max(),0)

syss_0_1_man_ar_forgym_min <- round(sysselsatta_vistelsetid_andel %>% filter(kön == "män",bakgrundsvariabel == "vistelsetid 0-1 år",utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$`Andel sysselsatta` %>% min(),0)
syss_0_1_man_ar_forgym_max <- round(sysselsatta_vistelsetid_andel %>% filter(kön == "män", bakgrundsvariabel == "vistelsetid 0-1 år",utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$`Andel sysselsatta` %>% max(),0)

# ============= Andel anställda i olika branscher uppdelat på kön (aggregerat över NMS) - motsvarar diagram 31 (sidan 36) i den tidigare rapporten
source(here("skript","socioek_andel_syss_bransch_diagram.R"), encoding="UTF-8")
andel_sysselsatta_kon_bransch <- funktion_upprepa_forsok_om_fel( function() {
  skapa_andel_anstallda_bransch_diagram(region_vekt = c("20", "17", "21"))
}, hoppa_over = hoppa_over_felhantering)
sysselsatta_bransch_kon_ar <- syss_bransch_andel_aggr$år %>% max()
sysselsatta_bransch_kon_manad <- syss_bransch_andel_aggr$månad_namn %>% unique()

sysselsatta_vard_kvinnor <- round(syss_bransch_andel_aggr %>% filter(Bransch == "Vård och omsorg",kön == "kvinnor") %>% .$andel,0)
sysselsatta_utbildning_kvinnor <- round(syss_bransch_andel_aggr %>% filter(Bransch == "Utbildning",kön == "kvinnor") %>% .$andel,0)

# ============= Överrepresentation av chefer - motsvarar diagram 32 i den tidigare rapporten
source(here("skript","socioek_overrep.R"), encoding="UTF-8")
overrepresentation_bransch <- funktion_upprepa_forsok_om_fel( function() {
  skapa_overrep_diagram()
}, hoppa_over = hoppa_over_felhantering)

overrep_ar_min <- overrep_df$Ar %>% min()
overrep_ar_max <- overrep_df$Ar %>% max()

# # ============= Ohälsotal uppdelat på region och kön - motsvarar diagram 33 (sidan 39) i den tidigare rapporten
# Gammalt, baserat på Kolada
# source(here("skript","socioek_ohalsa_diagram.R"), encoding="UTF-8")
# ohalsa_diagram <- funktion_upprepa_forsok_om_fel( function() {
#   skapa_ohalsotal_diagram(region_vekt = c("00", "20", "17", "21"))
# }, hoppa_over = hoppa_over_felhantering)
# ohalsa_ar <- ohalsa_df$ar %>% max()

# ============= Ohälsotal uppdelat på region och kön - motsvarar diagram 33 (sidan 39) i den tidigare rapporten
source(here("skript","socioek_ohalsa_diagram_korrekt.R"), encoding="UTF-8")
ohalsa_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_ohalsotal_lan(uppdatera_data = FALSE)
}, hoppa_over = hoppa_over_felhantering)
ohalsa_ar <- ohalsotal_df$År %>% max()
ohalsa_manad <- ohalsotal_df$månad_namn %>% unique()

ohalsotal_diff_man_max <- gsub("\\.",",",round(ohalsotal_df %>% filter(region != "Riket",Kön == "Män") %>% .$Ohalsotalet %>% max() - ohalsotal_df %>% filter(region == "Riket",Kön == "Män") %>% .$Ohalsotalet,0))
ohalsotal_diff_man_min <- gsub("\\.",",",round(ohalsotal_df %>% filter(region != "Riket",Kön == "Män") %>% .$Ohalsotalet %>% min() - ohalsotal_df %>% filter(region == "Riket",Kön == "Män") %>% .$Ohalsotalet,0))

ohalsotal_diff_kvinnor_max <- gsub("\\.",",",round(ohalsotal_df %>% filter(region != "Riket",Kön == "Kvinnor") %>% .$Ohalsotalet %>% max() - ohalsotal_df %>% filter(region == "Riket",Kön == "Kvinnor") %>% .$Ohalsotalet,0))
ohalsotal_diff_kvinnor_min <- gsub("\\.",",",round(ohalsotal_df %>% filter(region != "Riket",Kön == "Kvinnor") %>% .$Ohalsotalet %>% min() - ohalsotal_df %>% filter(region == "Riket",Kön == "Kvinnor") %>% .$Ohalsotalet,0))

  
# ============= Sjukpenningtal uppdelat på region och kön och ålder (2 diagram) motsvarar diagram 34 och 35 (sidan 39-40) i den tidigare rapporten
source(here("skript","socioek_sjukpenningtal_diagram.R"), encoding="UTF-8")
sjukpenningtal_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_sjukpenningtal_lan(uppdatera_data = FALSE)
}, hoppa_over = hoppa_over_felhantering)
sjukpenningtal_ar <- sjukpenningtal_totalt_df$År %>% max()
sjukpenningtal_manad <- sjukpenningtal_totalt_df$månad_namn %>% unique()

sjukpenningtal_varmland_man <- gsub("\\.",",",sjukpenningtal_totalt_df %>% filter(region == "Värmland", Kön == "Män") %>% .$sjukpenningtal)
sjukpenningtal_varmland_kvinnor <- gsub("\\.",",",sjukpenningtal_totalt_df %>% filter(region == "Värmland", Kön == "Kvinnor") %>% .$sjukpenningtal)

# =============  Pågående sjukfall uppdelat på diagnoskapitel motsvarar diagram 36 (sidan 40) i den tidigare rapporten
source(here("skript","socioek_pagaende_sjukfall_diagnos_diagram.R"), encoding="UTF-8")
pagaende_sjukfall_diagnos_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_pagaende_diagnos_lan(uppdatera_data = FALSE)
}, hoppa_over = hoppa_over_felhantering)
pagaende_sjukfall_ar <- sjukfall_diagnos_df$År %>% max()
pagaende_sjukfall_manad <- sjukfall_diagnos_df$månad_namn %>% unique()

pagaende_sjukfall_diagnos_kvinnor_riket <- round(sjukfall_diagnos_df %>% filter(region == "Riket", Kön == "Kvinnor",Diagnoskapitel == "Psykisk ohälsa") %>% .$`Andel`,0)
pagaende_sjukfall_diagnos_man_riket <- round(sjukfall_diagnos_df %>% filter(region == "Riket", Kön == "Män",Diagnoskapitel == "Psykisk ohälsa") %>% .$`Andel`,0)

# # =============  Självskattad hälsa, motsvarar diagram 37
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_sjalvskattad_halsa_kon_lan_fohm.R")
sjalvskattad_halsa_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_sjalvskattad_halsa_kon_lan(
    region_vekt = c("20","21", "17", "00"),
    region_sort = TRUE,
    ta_bort_diagramtitel = TRUE,
    returnera_dataframe_global_environment = TRUE,
    logga_path = NULL,
    tid_koder = "9999",
    diagram_capt = NULL
  )
}, hoppa_over = hoppa_over_felhantering)

sjalvskattad_halsa_ar <- sjalvskattad_halsa_df$År %>% max()

# ============= Låg ekonomisk standard uppdelat på sysselsatta, ålder och bakgrund - 3 diagram motsvarar diagram 40, 42 och 43 (sidor 45 - 48) i den tidigare rapporten
# Källa hårdkodad data innan diagram 33: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110I/TabVX4InkDesoN2/table/tableViewLayout1/
source(here("skript","socioek_lag_ek_standard_diagram.R"), encoding="UTF-8")
lag_ek_standard_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_ekonomiskstandard_lan()
}, hoppa_over = hoppa_over_felhantering)
lag_ek_standard_20_64_ar <- lag_ek_standard_20_64_df$år %>% max()
lag_ek_standard_alder_ar <- lag_ek_standard_alder_df$år %>% max()
lag_ek_standard_bakgrund_ar <- lag_ek_standard_bakgrund_df$år %>% max()

# Ålder
lag_ek_standard_80_varmland <- gsub("\\.",",",lag_ek_standard_alder_df %>% filter(region == "Värmland", ålder == "80- år") %>% .$`Inkomst < 60 procent`)
lag_ek_standard_80_gavleborg <- gsub("\\.",",",lag_ek_standard_alder_df %>% filter(region == "Gävleborg", ålder == "80- år") %>% .$`Inkomst < 60 procent`)
lag_ek_standard_80_Dalarna <- gsub("\\.",",",lag_ek_standard_alder_df %>% filter(region == "Dalarna", ålder == "80- år") %>% .$`Inkomst < 60 procent`)
lag_ek_standard_80_Riket <- gsub("\\.",",",lag_ek_standard_alder_df %>% filter(region == "Riket", ålder == "80- år") %>% .$`Inkomst < 60 procent`)

# Bakgrund
lag_ek_standar_max_varde_utrikes <- gsub("\\.",",",max(lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "utländsk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))
lag_ek_standar_max_varde_region_utrikes <- gsub("\\.",",",lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "utländsk bakgrund",sysselsättning == "Samtliga personer") %>% filter(`Inkomst < 60 procent`==max(`Inkomst < 60 procent`)) %>%.$region)

lag_ek_standar_min_varde_utrikes <- gsub("\\.",",",min(lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "utländsk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))
lag_ek_standar_min_varde_region_utrikes <- gsub("\\.",",",lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "utländsk bakgrund",sysselsättning == "Samtliga personer") %>% filter(`Inkomst < 60 procent`==min(`Inkomst < 60 procent`)) %>%.$region)

lag_ek_standar_riket_varde_utrikes <- gsub("\\.",",",min(lag_ek_standard_bakgrund_df %>% filter(region=="Riket", `utländsk/svensk bakgrund` == "utländsk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))

lag_ek_standar_max_varde_inrikes <- gsub("\\.",",",max(lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "svensk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))
lag_ek_standar_max_varde_region_inrikes <- gsub("\\.",",",lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "svensk bakgrund",sysselsättning == "Samtliga personer") %>% filter(`Inkomst < 60 procent`==max(`Inkomst < 60 procent`)) %>%.$region)

lag_ek_standar_min_varde_inrikes <- gsub("\\.",",",min(lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "svensk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))
lag_ek_standar_min_varde_region_inrikes <- gsub("\\.",",",lag_ek_standard_bakgrund_df %>% filter(region%in% c("Dalarna","Gävleborg","Värmland"), `utländsk/svensk bakgrund` == "svensk bakgrund",sysselsättning == "Samtliga personer") %>% filter(`Inkomst < 60 procent`==min(`Inkomst < 60 procent`)) %>%.$region)

lag_ek_standar_riket_varde_inrikes <- gsub("\\.",",",min(lag_ek_standard_bakgrund_df %>% filter(region=="Riket", `utländsk/svensk bakgrund` == "svensk bakgrund",sysselsättning == "Samtliga personer") %>% .$`Inkomst < 60 procent`))


# ============= Andel med ersättning av helårsekvivalenter - 3 diagram motsvarar diagram 41 (sidor 46) i den tidigare rapporten
source(here("skript","socioek_andel_helarsekvivalenter_diagram.R"), encoding="UTF-8")
helarsekvivalenter_andel_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_helarsekvivalenter_andel_lan()
}, hoppa_over = hoppa_over_felhantering)
ersattning_helarsakvivalenter_ar <- ersattning_helarsakvivalenter_df$år %>% max()
ersattning_helarsakvivalenter_manad <- ersattning_helarsakvivalenter_df$manad_namn %>% unique()
helarsekvivalenter_gavleborg <- gsub("\\.",",",round(ersattning_helarsakvivalenter_df %>% filter(region == "Gävleborg") %>% .$Andel_ersattning,1))
helarsekvivalenter_varmland <- gsub("\\.",",",round(ersattning_helarsakvivalenter_df %>% filter(region == "Värmland") %>% .$Andel_ersattning,1))
helarsekvivalenter_dalarna <- gsub("\\.",",",round(ersattning_helarsakvivalenter_df %>% filter(region == "Dalarna") %>% .$Andel_ersattning,1))
helarsekvivalenter_riket <- gsub("\\.",",",round(ersattning_helarsakvivalenter_df %>% filter(region == "Riket") %>% .$Andel_ersattning,1))

# ============= Sårbarhet (kommunnivå) - nytt diagram, det första under rubrik 6 Näringsliv
# Uppdateras ej automatiskt, kräver hämtning av data från NMS
source(here("skript","socioek_sarbarhet_ftg_NMS.R"), encoding="UTF-8")
sarbarhet_ftg_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_sarbarhet_ftg_diagram()
}, hoppa_over = hoppa_over_felhantering)
sarbarhet_ftg_ar <- sarbarhet_df$Ar %>% max()

# ============= Förändring sysselsatta - bubbeldiagram - diagram 45 i rapporten
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_rams_bas_syss_forandr_sni_branscher_bubblor_scb.R")
forandring_syss_branscher_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_forandr_branscher_bubblor(vald_geografi = c("17","20","21"),
                                 diagramtitel_tabort = TRUE,
                                 returnera_dataframe_global_environment = TRUE,
                                 till_word = TRUE,
                                 diagram_capt = NULL)
}, hoppa_over = hoppa_over_felhantering)
forandring_syss_branscher_ar <- forandring_syss_branscher_df$år %>% max()


# ============= Innovationsindex på region - motsvarar diagram 46 (sidan 52) i den tidigare rapporten
# Uppdateras ej automatiskt utan kräver nedladdning av Excel-fil
source(here("skript","socioek_innovationsindex_diagram.R"), encoding="UTF-8")
innovationsindex_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_innovationsindex_diagram()
}, hoppa_over = hoppa_over_felhantering)
innovationsindex_ar <- innovationsindex_df$År %>% max()

# ============= Omsättning och antal arbetsställen (grupperat på NMS) - motsvarar diagram 47 och 48 (sidor 54-55) i den tidigare rapporten
source(here("skript","socioek_intakter_arbstallen_diagram_ny.R"), encoding="UTF-8")
intakter_arbstallen_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_intakter_arbstallen_bransch_diagram(region_vekt = c("17", "20", "21"))
}, hoppa_over = hoppa_over_felhantering)
fek_ar <- fek_df$år %>% max()

oms_totalt <- (fek_df %>% filter(variabel ==  "Totala intäkter, mnkr") %>% group_by(år) %>% summarise(varde = sum(varde,na.rm=TRUE)) %>% .$varde)
andel_oms_industri <- round((fek_df %>% filter(variabel ==  "Totala intäkter, mnkr",Branschgrupp == "Tillverkning och utvinning") %>% .$varde/oms_totalt)*100,0)
andel_oms_jordbruk <- round((fek_df %>% filter(variabel ==  "Totala intäkter, mnkr",Branschgrupp == "Jordbruk och skogsbruk") %>% .$varde/oms_totalt)*100,0)

arb_totalt <- (fek_df %>% filter(variabel ==  "Antal arbetsställen (lokala verksamheter)") %>% group_by(år) %>% summarise(varde = sum(varde,na.rm=TRUE)) %>% .$varde)
andel_arbst_indstri <- round((fek_df %>% filter(variabel ==  "Antal arbetsställen (lokala verksamheter)",Branschgrupp == "Tillverkning och utvinning") %>% .$varde/arb_totalt)*100,0)
andel_arbst_jordbruk <- round((fek_df %>% filter(variabel ==  "Antal arbetsställen (lokala verksamheter)",Branschgrupp == "Jordbruk och skogsbruk") %>% .$varde/arb_totalt)*100,0)

# Tar hem data för sysselsatta för ovanstående branscher. Används på samma ställa i texten
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sysselsatta_bas_region_kon_sni2007_utbildningsniva_tid_ArRegUtb_AM0210F_scb.R")
syss <- hamta_sysselsatta_region_kon_sni2007_utbildningsniva_tid_scb(region_vekt = c("17", "20", "21"),
                                                                     kon_klartext = "totalt",
                                                                     sni2007_klartext = c("företag inom jordbruk, skogsbruk och fiske","tillverkningsindustri; gruvor och mineralutvinningsindustri"),
                                                                     cont_klartext = "sysselsatta efter arbetsställets belägenhet",
                                                                     tid_koder = fek_ar) %>% 
  group_by(år,`näringsgren SNI 2007`) %>% 
    summarise(antal = sum(`sysselsatta efter arbetsställets belägenhet`,na.rm=TRUE))
                                                                     
syss_jordbruk <- format(plyr::round_any(syss %>% filter(`näringsgren SNI 2007` == "företag inom jordbruk, skogsbruk och fiske") %>% .$antal,100),big.mark = " ")
syss_tillverkning <- format(plyr::round_any(syss %>% filter(`näringsgren SNI 2007` == "tillverkningsindustri; gruvor och mineralutvinningsindustri") %>% .$antal,100),big.mark = " ")
syss_ar <- syss$år %>% max()

# ============= Könsbalans per gymnasieprogram och län ===========================
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_gymn_elever_kon_prg_alla_arskurser_skolverket.R")
gymnasieprogram_konsbalans_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_gymn_elever_kon_prg_skolverket(region_vekt = c("17", "20", "21", "00"),
                                      gymnasieprogram = c("Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen"),
                                      ta_med_logga = FALSE,
                                      diagramrubrik_tabort = TRUE,
                                      returnera_data_rmarkdown = TRUE,
                                      skriv_diagramfil = FALSE)
}, hoppa_over = hoppa_over_felhantering) %>% .[[1]]
gymnasieprogram_konsbalans_ar <- gymn_elever_kon_prg_df$lasar %>% unique()

# ============= Genomoströmning i gymnasieskolan efter 4 år =======================================
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_gymn_genomstromning_4ar_prg_skolverket.R")
gymnasieprogram_genomstromning_diagram <- funktion_upprepa_forsok_om_fel( function() {
  diag_gymn_genomstromning_4ar_prg_skolverket(region_vekt = c("17", "20", "21", "00"),
                                      gymnasieprogram = c("Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen"),
                                      tid_koder = "9999",
                                      x_variabel = "region",
                                      y_variabel = "andel",
                                      x_grupp = "Gymnasieprogram",
                                      ta_med_logga = FALSE,
                                      diagramrubrik_tabort = TRUE,
                                      returnera_data_rmarkdown = TRUE,
                                      skriv_diagramfil = FALSE)
}, hoppa_over = hoppa_over_felhantering)
genomstromning_startlasar <- gymn_genomstromning_4ar_prg_df$läsår %>% unique()



# ============= Diagram från arbetsförmdelingen - lista med alla diagram som ska skapas ============= 
source(here("skript","socioek_af_diagram.R"), encoding="UTF-8")
af_lista_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_af_diagram_lista(region_vekt = c("17", "20", "21"),
                         visa_var_xe_etikett = 2)
}, hoppa_over = hoppa_over_felhantering)

# diagram över total andel arbetslösa över tid
arblos_arblosa_over_tid <- af_lista_diagram$linjediagram_andel_arblosa_over_tid
arblos_totalt_over_tid_min_ar <- arblosa_over_tid$Månad_år %>% first()
arblos_totalt_over_tid_max_ar <- arblosa_over_tid$Månad_år %>% last()

arblosa_over_tid_gavleborg <- gsub("\\.",",",round(arblos_arblosa_over_tid$data %>% filter(Region == "Gävleborg",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_over_tid_varmland <- gsub("\\.",",",round(arblos_arblosa_over_tid$data %>% filter(Region == "Värmland",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_over_tid_dalarna <- gsub("\\.",",",round(arblos_arblosa_over_tid$data %>% filter(Region == "Dalarna",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_over_tid_riket <- gsub("\\.",",",round(arblos_arblosa_over_tid$data %>% filter(Region == "Riket",Månad_år==last(Månad_år)) %>% .$total,1))

# diagram över andel arbetslösa utifrån tid i arbetslöshet, för senaste tillgängliga månad
arblos_arbloshetstid_diagram <- af_lista_diagram$andel_arblosa_arbloshetstid
arblos_arbloshetstid_ar <- arblosa_kon_arbloshetstid$Månad_år %>% unique()

# diagram över andel arbetslösa unga (18-24 år) över tid
arblos_arblosa_unga_over_tid <- af_lista_diagram$linjediagram_andel_unga_arblosa_over_tid
arblos_unga_min_ar <- arblosa_unga_over_tid$Månad_år %>% first()
arblos_unga_max_ar <- arblosa_unga_over_tid$Månad_år %>% last()

arblosa_unga_gavleborg <- gsub("\\.",",",round(arblos_arblosa_unga_over_tid$data %>% filter(Region == "Gävleborg",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_unga_varmland <- gsub("\\.",",",round(arblos_arblosa_unga_over_tid$data %>% filter(Region == "Värmland",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_unga_dalarna <- gsub("\\.",",",round(arblos_arblosa_unga_over_tid$data %>% filter(Region == "Dalarna",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_unga_riket <- gsub("\\.",",",round(arblos_arblosa_unga_over_tid$data %>% filter(Region == "Riket",Månad_år==last(Månad_år)) %>% .$total,1))

# diagram över andel arbetslösa utrikes födda över tid
arblos_arblosa_utrikes_over_tid <- af_lista_diagram$linjediagram_andel_utr_arblosa_over_tid
arblos_utr_min_ar <- arblosa_utr_over_tid$Månad_år %>% first()
arblos_utr_max_ar <- arblosa_utr_over_tid$Månad_år %>% last()

arblosa_utr_gavleborg <- gsub("\\.",",",round(arblos_arblosa_utrikes_over_tid$data %>% filter(Region == "Gävleborg",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_utr_varmland <- gsub("\\.",",",round(arblos_arblosa_utrikes_over_tid$data %>% filter(Region == "Värmland",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_utr_dalarna <- gsub("\\.",",",round(arblos_arblosa_utrikes_over_tid$data %>% filter(Region == "Dalarna",Månad_år==last(Månad_år)) %>% .$total,1)) 
arblosa_utr_riket <- gsub("\\.",",",round(arblos_arblosa_utrikes_over_tid$data %>% filter(Region == "Riket",Månad_år==last(Månad_år)) %>% .$total,1))


# ================== Diagram över matchad utbildningsnivå och bakgrund ============ 
source(here("skript","socioek_matchning_bakgr_utbniva.R"), encoding="UTF-8")
matchning_utbildningsniva_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_matcning_utbniva_bakgrund_diagram()
}, hoppa_over = hoppa_over_felhantering) %>% .[[1]]
matchning_utbniva_min_ar <- matchning_utbildningsniva_diagram$data$Ar %>% min()
matchning_utbniva_max_ar <- matchning_utbildningsniva_diagram$data$Ar %>% max()

matchning_utbniva_inrikes_ratt <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "Rätt utbildningsnivå",inr_utr_fodd == "Inrikes född",Ar == max(Ar)) %>% .$total,0))
matchning_utbniva_utrikes_ratt <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "Rätt utbildningsnivå",inr_utr_fodd == "Utrikes född",Ar == max(Ar)) %>% .$total,))

matchning_utbniva_inrikes_hog <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "För hög utbildningsnivå",inr_utr_fodd == "Inrikes född",Ar == max(Ar)) %>% .$total,0))
matchning_utbniva_utrikes_hog <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "För hög utbildningsnivå",inr_utr_fodd == "Utrikes född",Ar == max(Ar)) %>% .$total,))

matchning_utbniva_inrikes_lag <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "För låg utbildningsnivå",inr_utr_fodd == "Inrikes född",Ar == max(Ar)) %>% .$total,0))
matchning_utbniva_utrikes_lag <- gsub("\\.",",",round(matchning_utbildningsniva_diagram$data %>% filter(UtbNiva_match_txt == "För låg utbildningsnivå",inr_utr_fodd == "Utrikes född",Ar == max(Ar)) %>% .$total,))


# 2. om man vill knitta rapporten
#source(paste0(here("skript","/"), "2_knitta_rapport.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("skript","/"), "4_push_av_hela_repo_till_github.R"))


