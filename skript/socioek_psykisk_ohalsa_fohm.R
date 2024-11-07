if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
   			glue)

source("C:/gh/hamta_data/hamta_hlv_region_psykisk_halsa_andel_och_konfidensintervall_kon_ar_hlv1psyxreg_fohm.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")

diagram_capt <- "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
visa_dataetiketter <- FALSE
gg_list <- list()

hlv_df <- hamta_hlv_region_psykisk_halsa_andel_och_konfidensintervall_kon_ar_fohm(
			region_vekt = c("20", "17", "21", "00"),			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			psykisk_halsa_klartext = "*",			 #  Finns: "Gott psykiskt välbefinnande", "Allvarlig psykisk påfrestning**", "Nedsatt psykiskt välbefinnande", "Suicidtankar**", "Försökt ta sitt liv**", "Stressad", "Mycket stressad", "Sömnbesvär**", "Lätta sömnbesvär**", "Svåra sömnbesvär**", "Trötthet**", "Lätt trötthet**", "Svår trötthet**", "Ängslan, oro eller ångest**", "Lätt ängslan, oro eller ångest**", "Svår ängslan, oro eller ångest**", "Diagnosen depression av läkare"
			andel_och_konfidensintervall_klartext = "Andel",			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
			kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
			tid_koder = "*",			 #  Finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "hlv.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen

) %>% 
  rename(andel = last_col()) %>% 
  filter(!is.na(andel))

# om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
region_start <- unique(hlv_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
region_txt <- ar_alla_kommuner_i_ett_lan(unique(hlv_df$regionkod), returnera_text = TRUE, returtext = region_start)
region_txt <- ar_alla_lan_i_sverige(unique(hlv_df$regionkod), returnera_text = TRUE, returtext = region_txt)
regionfil_txt <- region_txt
region_txt <- paste0(" i ", region_txt)
regionkod_txt <- if (region_start == region_txt) unique(hlv_df$regionkod) %>% paste0(collapse = "_") else region_txt

valda_ar <- hlv_df %>%
  distinct(År) %>%   # Tar unika värden
  arrange(År) %>% 
  mutate(row_number = row_number()) %>%   # Skapar en radnummerkolumn
  filter(
    row_number == 1 |                    # Första elementet
      row_number == n() |                  # Sista elementet
      row_number == ceiling(n() / 3) |     # Ett element en tredjedel in
      row_number == ceiling(2 * n() / 3)   # Ett element två tredjedelar in
  ) %>%
  select(-row_number) %>% 
  dplyr::pull(År)

chart_df <- hlv_df %>% 
  filter(`Andel och konfidensintervall` == "Andel",
         Kön == "Totalt") %>% 
  filter(År %in% valda_ar) %>%
  filter(`Psykisk hälsa` %in% c("Ängslan, oro eller ångest**", "Sömnbesvär**", "Trötthet**")) %>% 
  mutate(region = region %>% skapa_kortnamn_lan(),
         region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")),
         `Psykisk hälsa` = `Psykisk hälsa` %>% str_remove("\\*\\*"))


diagramtitel <- glue("Psykisk hälsa (självrapporterat) efter region, kön och år")
diagramfil <- glue("hlv_{regionfil_txt}_ar{min(hlv_df$År)}_{max(hlv_df$År)}.png") %>% str_replace_all("__", "_")


gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
			 skickad_x_var = "Psykisk hälsa",
			 skickad_y_var = "andel",
			 skickad_x_grupp = "År",
			 x_axis_sort_value = FALSE,
			 diagram_capt = diagram_capt,
			 stodlinjer_avrunda_fem = TRUE,
			 filnamn_diagram = diagramfil,
			 dataetiketter = visa_dataetiketter,
			 lagg_pa_logga = FALSE,
			 manual_y_axis_title = "",
			 x_axis_lutning = 0,
			 # manual_x_axis_text_vjust = 1,
			 # manual_x_axis_text_hjust = 1,
			 manual_color = rep(diagramfarger("rus_sex"),3),
			 output_mapp = output_mapp,
			 diagram_facet = TRUE,
			 facet_grp = "region",
			 facet_scale = "free_x",
			 facet_legend_bottom = TRUE
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")

