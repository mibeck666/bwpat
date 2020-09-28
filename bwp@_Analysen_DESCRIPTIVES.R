######################################################
##Syntax:   Descritptive Analysis
##Project:  bwp@ Article
##Owner: IBG / IPK
##Author:    bmc
#R Version: 3.6.1
#R Studio Version 1.2.1335 
######################################################

####################
#Proportions / Sample
#Geschlecht
full_data %>% group_by(Studie, Geschlecht) %>% 
  filter(!is.na(Geschlecht)) %>% 
  summarise(n = n()) %>% 
  mutate(Prozent = n / sum(n) * 100)

#Migration
full_data %>% group_by(Studie, mighin) %>% 
  filter(!is.na(mighin)) %>% 
  summarise(n = n()) %>% 
  mutate(Prozent = n / sum(n) * 100)

#Klassenstufe
full_data %>% group_by(Studie, Stufe) %>% 
  summarise(n = n()) %>% 
  mutate(Prozent = n / sum(n) * 100)


full_data %>% group_by(Studie) %>% 
  filter(!is.na(Alter)) %>% 
  summarise(MW = mean(Alter, na.rm = TRUE),
            Med = median(Alter, na.rm = TRUE),
            SD = sd(Alter, na.rm = TRUE),
            Max = max(Alter, na.rm = TRUE),
            Min = min(Alter, na.rm = TRUE)) 

miau <- full_data %>% 
 # mutate(Studie = factor(Studie)) %>% 
  group_by(Studie) %>% 
  summarise_at(vars(starts_with("wert_")), mean, na.rm = TRUE)

miau_n <- full_data %>% 
  # mutate(Studie = factor(Studie)) %>% 
  group_by(Studie) %>% 
  select(Studie, starts_with("wert")) %>% 
  summarise_all(funs(sum(!is.na(.))))

miau <- bind_rows(miau, miau_n)

wuff <- full_data %>% 
  # mutate(Studie = factor(Studie)) %>% 
  group_by(Studie) %>% 
  summarise_at(vars(starts_with("ber")), mean, na.rm = TRUE)

wuff_n <- full_data %>% 
  # mutate(Studie = factor(Studie)) %>% 
  group_by(Studie) %>% 
  select(Studie, starts_with("ber")) %>% 
  summarise_all(funs(sum(!is.na(.))))

wuff <- bind_rows(wuff, wuff_n)

mienz <- as_tibble(cbind(rownames(t(miau[, 2:ncol(miau)])), t(miau[, 2:ncol(miau)])))
wau <- as_tibble(cbind(rownames(t(wuff[, 2:ncol(wuff)])), t(wuff[, 2:ncol(wuff)])))


colnames(mienz) <- c("Variable", "Berufslernende", "Oberstufe", "N_Berufslernende", "N_Oberstufe")
colnames(wau) <- c("Variable", "Berufslernende", "Oberstufe", "N_Berufslernende", "N_Oberstufe")

table_values_full <- mienz %>% mutate_at(vars(Berufslernende, Oberstufe), funs(as.numeric)) %>% 
  arrange(desc(Oberstufe)) %>% 
  mutate('Rang Oberstufe' = 1:nrow(.)) %>% 
  arrange(desc(Berufslernende)) %>% 
  mutate('Rang Berufslernende' = 1:nrow(.)) %>% 
  mutate(Variable = dplyr::recode(Variable,
                                  wert_1 = 'Gesundheitsbewusstsein',
                                  wert_2 = 'Leben geniessen',
                                  wert_3 = 'Umweltbewusststein',
                                  wert_23 = 'Gute Ausbildung',
                                  wert_24 = 'Gute Beziehungen',
                                  wert_4 = 'Macht und Einfluss',
                                  wert_5 = 'Gesetz und Ordnung',
                                  wert_6 = 'Sicherheit',
                                  wert_7 = 'Fleiss und Ehrgeiz',
                                  wert_8 = 'Hoher Lebensstandard',
                                  wert_9 = 'Durchsetzungskraft',
                                  wert_10 = 'Phantasie und Kreativität',
                                  wert_11 = 'Sozial Benachteiligten und gesellschaftlichen Randgruppen helfen',
                                  wert_12 = 'Abweichende Meinungen tolerieren',
                                  wert_13 = 'Politisches Engagement',
                                  wert_14 = 'Unabhängigkeit',
                                  wert_25 = 'Abwechslungsreichtum und Aufregung',
                                  wert_15 = 'An Traditionen halten',
                                  wert_16 = 'Tun, was andere tun.',
                                  wert_17 = 'Eigenverantwortlich leben und handeln.',
                                  wert_18 = 'Gutes Familienleben',
                                  wert_19 = 'Partner*in vertrauen',
                                  wert_20 = 'Akzeptanz und Anerkennung Freund*innen',
                                  wert_21 = 'Viele Kontakte',
                                  wert_22 = 'Vielfalt anerkennen und akzeptieren'))


table_values <- table_values_full %>% 
  select(Variable, 'Rang Berufslernende', 'Rang Oberstufe')
  
  
table_job_full <- wau %>% mutate_at(vars(Berufslernende, Oberstufe), funs(as.numeric)) %>% 
  arrange(desc(Oberstufe)) %>% 
  mutate('Rang Oberstufe' = 1:nrow(.)) %>% 
  arrange(desc(Berufslernende)) %>% 
  mutate('Rang Berufslernende' = 1:nrow(.)) %>% 
  mutate(Variable = dplyr::recode(Variable, berpraef_1 = 'Spass',
                                  berpraef_17 = 'Geld',
                                  berpraef_2 = 'Verantwortung',
                                  berpraef_13 = 'Prestige',
                                  berpraef_7 = 'Arbeitsplatzsicherheit',
                                  berpraef_4 = 'Karriere',
                                  berpraef_3 = 'Zeit für Hobbies',
                                  berpraef_12 = 'Sinnvolles für Gesellschaft',
                                  berpraef_9 = 'Zeit für Familie'))
  

table_job <- table_job_full %>% 
  select(Variable , 'Rang Berufslernende', 'Rang Oberstufe')


write_excel_csv2(table_values, file.path(pfad_tab, "Tabelle_1.csv"))
write_excel_csv2(table_values_full, file.path(pfad_tab, "Tabelle_1_descriptives.csv"))

write_excel_csv2(table_job, file.path(pfad_tab, "Tabelle_2.csv"))
write_excel_csv2(table_job_full, file.path(pfad_tab, "Tabelle_2_descriptives.csv"))

