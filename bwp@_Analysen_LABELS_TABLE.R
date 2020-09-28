################################
#TABLES Question Wordings goals
################################

#Names of Files containing Labels
names_labels_lebel <- "LEBEL_Codebuch_180718.xlsx"
names_labels_leoj <- "LeOJ_Variablen.xlsx"

#Read Files containing Labels

label_lebel <- read_excel(file.path(pfad, names_labels_lebel), sheet = "BL und HFS") %>%
  filter(str_detect(Variablenname, "wert_\\d|berpraef_\\d")) %>% 
  select(var_name_lebel = Variablenname,
         var_label_lebel = Variablenlabel) %>% 
  mutate(var_label_lebel = str_replace_all(var_label_lebel,
                                           pattern = "Mir persönlich ist in meinem Berufsleben wichtig, dass |Mir persönlich ist in meinem Leben wichtig, dass ich ",
                                           ""))

points <- str_sub(label_lebel$var_label_lebel[1], 1, 1)        #Extract ..., that are obiously one string

label_lebel <- label_lebel %>% 
  mutate(var_label_lebel = str_replace_all(var_label_lebel,
                                           pattern = points,   #Get rid of the one string three points
                                           "..."))             #Add the probper three points

label_leoj <- read_excel(file.path(pfad, names_labels_leoj), sheet = "VariableView") %>% 
  filter(str_detect(`Variable Name`, "VAR07_\\d|VAR33_\\d")) %>% 
  select(var_name_leoj = "Variable Name",
         var_label_leoj = "Label") %>% 
  mutate(var_label_leoj = str_replace_all(var_label_leoj,
                                          pattern = "Mir persönlich ist in meinem Leben wichtig, .... - dass ich |Was wäre dir bei einer zukünftigen Arbeitsstelle wichtig? - ",
                                          "... "),
 #ÖNEW        var_name_lebel = dplyr::recode(var_name_leoj,
                                        'VAR07_1' = 'wert_1',
                                        'VAR07_2' = 'wert_2',
                                        'VAR07_3' = 'wert_3',
                                        'VAR07_4' = 'wert_23',
                                        'VAR07_5' = 'wert_24',
                                        'VAR07_6' = 'wert_4',
                                        'VAR07_7' = 'wert_5',
                                        'VAR07_8' = 'wert_6',
                                        'VAR07_9' = 'wert_7',
                                        'VAR07_10' = 'wert_8',
                                        'VAR07_11' = 'wert_9',
                                        'VAR07_12' = 'wert_10',
                                        'VAR07_13' = 'wert_11',
                                        'VAR07_14' = 'wert_12',
                                        'VAR07_15' = 'wert_13',
                                        'VAR07_16' = 'wert_14',
                                        'VAR07_17' = 'wert_25',
                                        'VAR07_18' = 'wert_15',
                                        'VAR07_19' = 'wert_16',
                                        'VAR07_20' = 'wert_17',
                                        'VAR07_21' = 'wert_18',
                                        'VAR33_1' = 'berpraef_1',
                                        'VAR33_2' = 'berpraef_17',
                                        'VAR33_3' = 'berpraef_2',
                                        'VAR33_4' = 'berpraef_13',
                                        'VAR33_5' = 'berpraef_7',
                                        'VAR33_6' = 'berpraef_4',
                                        'VAR33_7' = 'berpraef_3',
                                        'VAR33_8' = 'berpraef_12',
                                        'VAR33_9' = 'berpraef_9')) 




label_table_values <- left_join(label_leoj, label_lebel) %>% 
  filter(str_detect(var_name_lebel, "ber", negate = TRUE)) %>% 
  mutate(Wert = dplyr::recode(var_name_lebel,
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
                              wert_11 = 'Benachteiligten und Randgruppen helfen',
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
                              wert_22 = 'Vielfalt anerkennen und akzeptieren')) %>% 
  select(Wert,
         'Text LEBEL' = var_label_lebel,
         'Text LeOJ' = var_label_leoj)

  