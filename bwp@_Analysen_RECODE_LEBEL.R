######################################################
##Syntax:   RECODING and SELECTING LeOJ
##Project:  bwp@ Article
##Owner: IBG / IPK
##Author:    bmc
#R Version: 3.6.1
#R Studio Version 1.2.1335 
######################################################

###############
#Read Datafiles
lebel.raw <- read_sav(file.path(pfad, lebel.name)) 


#Helper Vector Covariates for handling missing data
pred_reg <-c("code_KL", "Alter", "Geschlecht", "mighin", "HISCED", "HISEI", "Studie")


#Revert all DV: Coding for better intepretation (Higher Value = higher goals)
lebel.recode <-  lebel.raw %>% 
  #Make it a tibble, for dplyr use
  as_tibble() %>%
  mutate(bild_va = ifelse(is.na(Ausb_vat), 1, Recode(Ausb_vat_2, "1:4 = 2; 5:6 = 3; 7:8 = 4; 9 = 1")),
         bild_mu = ifelse(is.na(Ausb_mut), 1, Recode(Ausb_mut_2, "1:4 = 2; 5:6 = 3; 7:8 = 4; 9 = 1"))) %>%
  mutate(HISCED = factor(pmax(bild_va, bild_mu, na.rm = TRUE),
                         labels = c("weiss nicht / fehlend", "obligatorisch", "Sek II (Lehre oder Matur)", "Tertiär (Studium, höhere Berufsausbildung")),
         Studie = "Auszubildende",
         code_KL = as.character(code_KL),
         mighin = factor(mig_MH1, labels = c("CH", "2. Gen.", "1. Gen.")),
         Geschlecht = factor(Geschlecht, labels = c("weiblich", "männlich")),
         Stufe = Lehrjahr)

lebel.ana <- lebel.recode %>% 
  #Select variables
  select(pred_reg, wert_1, wert_8, wert_9, wert_10, wert_11, wert_12, wert_13, wert_14, 
         wert_25, wert_15, wert_2, wert_16, wert_17, wert_3, wert_23, wert_24,
         wert_4, wert_5, wert_6, wert_7, berpraef_1, berpraef_17, berpraef_2,
         berpraef_13, berpraef_7, berpraef_4, berpraef_3, berpraef_12, berpraef_9) %>% 
  mutate_at(vars(starts_with("wert"), starts_with("berpraef")), as.double)

rm(lebel.recode, lebel.raw)
