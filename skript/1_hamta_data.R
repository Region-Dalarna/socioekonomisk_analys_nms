
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# tabell över befolkningsutvecklingen i NMS-länen samt riket per åldersgrupp
source(here("skript","socioek_beftabell.R"), encoding="UTF-8")
beftabell_flextable <- funktion_upprepa_forsok_om_fel( function() {
  skapa_bef_tabell()
})

# diagram över befolkningsutvecklingen
source(here("skript","socioek_befutv_diagram.R"), encoding="UTF-8")
befutv_linjediagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_befutv_diagram(region_vekt = c("17", "20", "21"))
})
bef_utv_slutar <- befutv_linjediagram$data$år %>% max()


# diagram över befolkningsförändringar per län i NMS
source(here("skript","socioek_befforandr_diagram.R"), encoding="UTF-8")
befforandringar_diagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_beffor_diagram(region_vekt = c("17", "20", "21"))
})
beffor_ar <- beffor_df$år %>% max()


# diagram över invandring per län i NMS från år 2000 och framåt - linjediagram
source(here("skript","socioek_utrinflyttning_diagram.R"), encoding="UTF-8")
inv_nms_linjediagram <- funktion_upprepa_forsok_om_fel( function() {
  skapa_invandring_diagram(region_vekt = c("17", "20", "21"))
})
inv_ar <- inv_nms_linjediagram$data$år %>% max

# 2. om man vill knitta rapporten
#source(paste0(here("skript","/"), "2_knitta_rapport.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("skript","/"), "4_push_av_hela_repo_till_github.R"))


