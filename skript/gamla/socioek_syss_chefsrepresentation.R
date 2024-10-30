library(pxweb)
library(httr)
library(dplyr)
library(tidyr)
library(purrr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

# ========================================== Inställningar ============================================

mapp <- "G:/Samhällsanalys/Projekt och uppdrag/EU/ESF/Socioekonomisk analys/auto_diagram/"
nyckelmapp <- "G:/skript/nycklar/"
branschnyckelfil <- "Bransch_Gxx_farger.csv"

region_filter <- c("20", "17", "21")
slut_ar <- NA             # NA för senaste möjliga år 
start_ar <- NA            # NA för tidigast möjliga år

#==========================================================================================================
# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

# =============================================== API-uttag ===============================================

tablist <- list(url = list("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG56N",
                         "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG56",
                         "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG2107"),
                content = list("000003T3", "000000PJ", "AM0208D3"),
                yrkevar = list("Yrke2012", "Yrke2012", "Yrke"))

px_df <- NULL
px_alla <- NULL



# samtliga år för tabellerna i tablist
alla_ar_list <- map(tablist[["url"]], ~hamta_giltig_tid_tabell(., tidkol = "år"))        # skapa en lista med alla år per tabell
alla_ar_unika <- unique(unlist(alla_ar_list))                                            # skapa en vektor med alla unika år 

# ta ut första och sista år 
min_ar <- min(alla_ar_unika)
max_ar <- max(alla_ar_unika)

# om startår saknas tas första möjliga år, om startår är tidigare än tidigaste år så tas ändå tidigaste tillgängliga år
if (is.na(start_ar)) start_ar <- min_ar else {
  if (start_ar < min_ar) start_ar <- min_ar
}
# om slutår saknas tas första möjliga år, om slutår är senare än senast tillgängliga år så tas ändå senaste tillgängliga år
if (is.na(slut_ar)) slut_ar <- max_ar else {
  if (slut_ar > max_ar) slut_ar <- max_ar
}

# kontrollera att slutår är större än startår
if (slut_ar < start_ar){
  temp <- slut_ar
  slut_ar <- start_ar
  start_ar <- temp 
}


for (tab in 1:length(tablist[[1]])){
  
  ar_i_denna_tabell <- intersect(c(start_ar, slut_ar), alla_ar_list[[tab]])
  
    if (length(ar_i_denna_tabell)>0){
      varlista <- list(
      Region = region_filter,
      Yrke = '*',
      SNI2007 = '*',
      Kon = '*',
      ContentsCode = tablist[["content"]][[tab]],
      Tid = ar_i_denna_tabell
      )
      # byt namn så att yrkesvariabeln blir rätt
      names(varlista)[[2]] <- tablist[["yrkevar"]][[tab]]
    
    # API-uttaget
    px_uttag <- pxweb_get(url = tablist[["url"]][[tab]], query = varlista) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(regionkod = Region, branschkod = SNI2007, yrkeskod = all_of(tablist[["yrkevar"]][[tab]])))  
    
    px_df <- px_df %>% 
      rename(syss = all_of(which(grepl("nställd", names(px_df))))) %>%
      rename(yrke = 2) %>%
      relocate(regionkod, .before = region) %>% 
      relocate(branschkod, .before = `näringsgren SNI 2007`) %>% 
      relocate(yrkeskod, .before = all_of(3))
  
    px_alla <- rbind(px_alla, px_df)
  }
  
}

branschnyckel <- read.csv2(paste0(nyckelmapp, branschnyckelfil))

chef_df <- left_join(px_alla, branschnyckel %>% select(Br15kod, Bransch), by = c("branschkod" = "Br15kod"))

chef_df2 <- chef_df %>%
  mutate(chef = ifelse(substr(yrkeskod,1,1) == "1", "chefer", "övriga")) %>% 
  group_by(år, Bransch, chef, kön) %>% 
  summarise(antal = sum(syss)) %>% 
  mutate(andel_chef = round((antal / sum(antal))*100,1)) %>% 
  ungroup()

chef_df3 <- chef_df2 %>% 
  group_by(år, Bransch) %>% 
  mutate(antal_tot = sum(antal)) %>% 
  ungroup() %>% 
  group_by(år, Bransch, kön) %>% 
  mutate(antal_kon = sum(antal),
         andel_bransch = round((antal_kon / antal_tot)*100,1),
         diff_chef_tot = (andel_chef-andel_bransch) / andel_bransch) %>% 
  ungroup() %>% 
  filter(kön == "män") %>% 
  filter(chef == "chefer")


# =========================== Skapa diagram ==============================



# Diagram med jämförelse mellan länen för senaste år
diagramfil <- "diagram_28_syss_chefer_kon.png"

SkapaStapelDiagram(skickad_df = chef_df3,
                   skickad_x_var = "Bransch",
                   skickad_y_var = "diff_chef_tot",
                   skickad_x_grupp = "år",
                   #manual_color = diagramfarger("kon"),
                   #x_axis_lutning = 0,
                   #diagram_liggande = TRUE,
                   x_axis_sort_value = TRUE,
                   #y_axis_100proc = TRUE,
                   #geom_position_stack = TRUE,
                   manual_y_axis_title = "överrepresentation av manliga chefer",
                   facet_legend_bottom = TRUE,
                   #x_var_fokus = "fokus",
                   manual_x_axis_text_vjust = 1,
                   manual_x_axis_text_hjust = 1,
                   lagg_pa_logga = FALSE,
                   output_mapp = mapp,
                   filnamn_diagram = diagramfil)

 

# ===================== Interaktiv sökning i statistikdatabasen ========================

# För att interaktivt söka i pxweb-databaser
#pxweb_interactive()

# Interaktiv sökning - direktingång till SCB:s statistikdatabas
#pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")

#pxweb_interactive(tablist[["url"]][[tab]])
