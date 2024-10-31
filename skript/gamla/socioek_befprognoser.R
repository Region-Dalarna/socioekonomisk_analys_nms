
source("G:/skript/peter/befprognos/func_BefPrognos.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/peter/befprognos/BefPrognos_installningar.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/peter/befprognos/BefPrognos_InrUtrFodda_installningar.R", encoding = "utf-8", echo = FALSE)

vald_region <- c("20", "17", "21")
vald_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"

regionnamn <- "Norra Mellansverige"

befprogn_diag <- SkapaBefPrognosDiagram(aktlan = vald_region, 
                        output_fold = vald_mapp,       # mapp på datorn som diagrammet skrivs till
                        skapa_fil = TRUE,
                        bara_lan = TRUE,                          # TRUE om bara län ska visas, FALSE för att visa länets kommuner
                        AktuellRegion = "Norra Mellansverige",                     # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                        jmfrtid = 10,                             # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
                        JmfrFleraPrognoser = TRUE,               # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                        gruppera = TRUE,                         # TRUE om vi vill lägga ihop flera regioner, FALSE om vi vill skriva ett diagram per region  
                        anvand_senaste_befar = TRUE,              # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                        utan_diagramtitel = FALSE,             # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                        ta_med_logga = TRUE,                     # TRUE om vi vill ha med logga, annars FALSE
                        logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                        diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna", 
                        logga_storlek = 25,  
                        dataetiketter = FALSE,                    # TRUE om vi vill skriva ut värdena i diagrammet
                        facet_scale = "fixed",
                        manualfarg = diagramfarger("rd_gron")[c(4,2,1)]
                        )


vald_region <- c("01", "20", "17", "21")
vald_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"

regionnamn <- "Norra Mellansverige och Stockholm"

befprogn_diag <- SkapaBefPrognosDiagram(aktlan = vald_region, 
                                         output_fold = vald_mapp,       # mapp på datorn som diagrammet skrivs till
                                         skapa_fil = TRUE,
                                         bara_lan = TRUE,                          # TRUE om bara län ska visas, FALSE för att visa länets kommuner
                                         AktuellRegion = regionnamn,                     # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                         jmfrtid = 10,                             # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
                                         JmfrFleraPrognoser = FALSE,               # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                                         gruppera = FALSE,                         # TRUE om vi vill lägga ihop flera regioner, FALSE om vi vill skriva ett diagram per region  
                                         anvand_senaste_befar = TRUE,              # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                         utan_diagramtitel = FALSE,             # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                         ta_med_logga = TRUE,                     # TRUE om vi vill ha med logga, annars FALSE
                                         logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                         diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna", 
                                         logga_storlek = 15,  
                                         dataetiketter = FALSE,                    # TRUE om vi vill skriva ut värdena i diagrammet
                                         facet_scale = "free",
                                         manualfarg = diagramfarger("rd_gron")[1]
)




# 
# 
# 
# befprogn_diag <- diag_befolkningsprognos_inr_utr_fodda(region_vekt = vald_region, 
#                                       bara_lan = TRUE, 
#                                       jmfrtid = 10, 
#                                       dataetiketter = FALSE,
#                                       utan_diagramtitel = FALSE,
#                                       ta_med_logga = TRUE,
#                                       skapa_fil = TRUE, 
#                                       output_fold = "G:/Samhällsanalys/API/Fran_R/Utskrift/",     # mapp på datorn som diagrammet skrivs till
#                                       logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
#                                       diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Peter Möller, Region Dalarna",
#                                       logga_storlek = 15
#                                       )
