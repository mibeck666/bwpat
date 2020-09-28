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
leoj.raw <- read_sav(file.path(pfad, leoj.name))

#Recode Education, Immigration History and Factor the Factors
#LeOJ#
#############################################################
leoj.recode <- leoj.raw %>%
  #Recode Educational Credentials Parents
  mutate(bild_va = Recode(VAR56, "3:5 = 2; 1:2 = 3; 6:8 = 4; 0 = 1; 9 = 1"),
         bild_sva = ifelse(is.na(VAR56), Recode(VAR57, "3:5 = 2; 1:2 = 3; 6:8 = 4; 0 = 1; 9 = 1"), NA),
         bild_mu =  Recode(VAR58, "3:5 = 2; 1:2 = 3; 6:8 = 4; 0 = 1; 9 = 1"),
         bild_smu = ifelse(is.na(VAR58), Recode(VAR59, "3:5 = 2; 1:2 = 3; 6:8 = 4; 0 = 1; 9 = 1"), NA),
         #Recode Educational Aspirations Dummy 1 = Apprenticeship, 0 = 
         ausbildung = Recode(VAR30, "1 = 0; 2:3 = 1; 4:6 = 0; 7:8 = 1; 9 = 0")) %>%
  #Highest Educational Credential of any parent and labelling,
  mutate(HISCED = factor(pmax(bild_va, bild_sva, bild_mu, bild_smu, na.rm = TRUE),
                         labels = c("weiss nicht / fehlend", "obligatorisch", "Sek II (Lehre oder Matur)", "Tertiär (Studium, höhere Berufsausbildung")),
         #Immigrant History as factor and labelling
         mig_3 = factor(Recode(mig_hin, "1:3 = 1; 4 = 2; 5 = 3"), labels = c("CH", "2. Gen.", "1. Gen.")),
         #AGE contains unplausible values (=21 years), set to missing
         Alter = ifelse(Alter == 21, NA, Alter),
         HISEI = ISEI_Eltern_Hi,
         Studie = "Schüler*innen Oberstufe",
         Geschlecht = factor(Recode(Geschlecht, "1 = 2; 2 = 1"), labels = c("weiblich", "männlich")),
         Stufe = Recode(Stufe, "1 = 7; 2 = 8; 3 = 9")) %>% 
  filter(ausbildung == 1)



#Create "Helper"-Vectors for quick indexing
#########################

#Helper-Vectors life goals 
var_lg <- c("VAR07_1", "VAR07_2", "VAR07_3", "VAR07_4", "VAR07_5", "VAR07_6",
            "VAR07_7", "VAR07_8", "VAR07_9", "VAR07_10", "VAR07_11", "VAR07_12",
            "VAR07_13", "VAR07_14", "VAR07_15", "VAR07_16", "VAR07_17", "VAR07_19", 
            "VAR07_20", "VAR07_21")


#Helper-Vectors SPF /  job goals

var_jg <- c("VAR33_1", "VAR33_2", "VAR33_3", "VAR33_4", "VAR33_5",
            "VAR33_6", "VAR33_7", "VAR33_8", "VAR33_9")

#Revert all DV: Coding for better intepretation (Higher Value = higher goals)
leoj.rev <- sapply(leoj.recode[, c(var_jg, var_lg)], function(x){x <- car::recode(x,"1 = 4; 2 = 3; 3 = 2; 4 = 1"); x}) %>%
  #Make it a tibble, for dplyr use
  as_tibble() %>%
  #Rename (to LEBEL names) and select variables
  select(wert_1 = VAR07_1,	
         wert_8 = VAR07_10,	
         wert_9 = VAR07_11,	
         wert_10 = VAR07_12,	
         wert_11 = VAR07_13,	
         wert_12 = VAR07_14,	
         wert_13 = VAR07_15,	
         wert_14 = VAR07_16,	
         wert_25 = VAR07_17,	
         wert_15 = VAR07_19,	
         wert_2 = VAR07_2,	
         wert_16 = VAR07_20,	
         wert_17 = VAR07_21,	
         wert_3 = VAR07_3,	
         wert_23 = VAR07_4,	
         wert_24 = VAR07_5,	
         wert_4 = VAR07_6,	
         wert_5 = VAR07_7,	
         wert_6 = VAR07_8,	
         wert_7 = VAR07_9,	
         berpraef_1 = VAR33_1,	
         berpraef_17 = VAR33_2,	
         berpraef_2 = VAR33_3,	
         berpraef_13 = VAR33_4,	
         berpraef_7 = VAR33_5,	
         berpraef_4 = VAR33_6,	
         berpraef_3 = VAR33_7,	
         berpraef_12 = VAR33_8,	
         berpraef_9 = VAR33_9)

#Helper Vector Covariates for handling missing data
pred_reg <-c("Kl_Code", "Alter", "Geschlecht", "mig_3", "HISCED", "HISEI", "Studie")


#Bind reverted values with explanatory variables
leoj.ana <- bind_cols(leoj.recode[, pred_reg], leoj.rev) %>% 
  rename(mighin = mig_3,
         code_KL = Kl_Code)



rm(var_lg, var_jg, leoj.raw, leoj.recode, leoj.rev)
