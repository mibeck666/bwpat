###############
##Graphical Display of Posterior means and credible intervals

###################################
#DF Preparation for Plotting 

###################
#Values
#Create ordered DF (ordered by Value, grouped by Study)
plot_frame <- n_frame_func(table_coeff_values) 

plot_frame <- plot_frame %>% #Learn how to sort with stackoverflow https://stackoverflow.com/questions/60814228/how-to-sort-a-dataframe-in-r-by-one-variable-while-grouping-for-others
    filter(term == "StudieLernende") %>%
  mutate(ranking = dense_rank(desc(estimate))) %>%
  bind_rows(plot_frame %>%
              filter(term == "StudieOberstufe")) %>%
  group_by(value) %>%
  fill(ranking) %>%
  arrange(ranking) %>%
  mutate(term = str_remove(term, "Studie")) %>% 
  mutate(Studie = as.factor(term))


#Make proper Labels for Plots
plot_frame <- plot_frame %>%
  mutate(Variable = dplyr::recode(value, wert_1 = 'Gesundheits-ybewusstsein',
                                  wert_2 = 'Leben geniessen',
                                  wert_3 = 'Umwelt-ybewusststein',
                                  wert_23 = 'Gute Ausbildung',
                                  wert_24 = 'Gute Beziehungen',
                                  wert_4 = 'Macht und yEinfluss',
                                  wert_5 = 'Gesetz und yOrdnung',
                                  wert_6 = 'Sicherheit',
                                  wert_7 = 'Fleiß und yEhrgeiz',
                                  wert_8 = 'Hoher yLebensstandard',
                                  wert_9 = 'Durchsetzungskraft',
                                  wert_10 = 'Phantasie und yKreativität',
                                  wert_11 = 'Benachteiligten und yRandgruppen helfen',
                                  wert_12 = 'Abweichende Mein-yungen tolerieren',
                                  wert_13 = 'Politisches yEngagement',
                                  wert_14 = 'Unabhängigkeit',
                                  wert_25 = 'Abwechslungsreichtum yund Aufregung',
                                  wert_15 = 'An Traditionen yhalten',
                                  wert_16 = 'Tun, was yandere tun.',
                                  wert_17 = 'Eigenverantwortlich yleben und handeln.'),
         Studie = recode_factor(Studie, Lernende = "Auszubildende", Oberstufe = "Schüler*innen Oberstufe")) 
  



##Plots Values
#Abbildung 1:
p1 <- plot_bwpat(plot_frame, n = 5, s = 1) 
p2 <- plot_bwpat(plot_frame, n = 5, s = 11)
p3 <- plot_bwpat(plot_frame, n = 5, s = 21)
p4 <- plot_bwpat(plot_frame, n = 5, s = 31)
ggarrange(p1, p2, p3, p4, nrow = 4, common.legend = TRUE, legend = "bottom")
ggsave(file.path(plot_path, "/Abb3.png"), width = 7.07, height = 10.76)



#################
###################
#Job Goals
#Create ordered DF (ordered by Value, grouped by Study)
plot_frame_j <- n_frame_func(table_coeff_job) 

plot_frame_j <- plot_frame_j %>%
  filter(term == "StudieLernende") %>%
  mutate(ranking = dense_rank(desc(estimate))) %>%
  bind_rows(plot_frame_j %>%
              filter(term == "StudieOberstufe")) %>%
  group_by(value) %>%
  fill(ranking) %>%
  arrange(ranking) %>%
  mutate(term = str_remove(term, "Studie")) %>% 
  mutate(Studie = as.factor(term))

#Make proper Labels for Plots
plot_frame_j <- plot_frame_j %>%
  mutate(Variable = dplyr::recode(value, berpraef_1 = 'Spaß',
                                  berpraef_17 = 'Geld',
                                  berpraef_2 = 'Verantwortung',
                                  berpraef_13 = 'Prestige',
                                  berpraef_7 = 'Arbeitsplatzsicherheit',
                                  berpraef_4 = 'Karriere',
                                  berpraef_3 = 'Zeit für Hobbies',
                                  berpraef_12 = 'Sinnvolles für yGesellschaft',
                                  berpraef_9 = 'Zeit für Familie'),
         Studie = recode_factor(Studie, Lernende = "Auszubildende", Oberstufe = "Schüler*innen Oberstufe"))

##Plots Job Goals
#Abbildung 3:
p6 <- plot_bwpat(plot_frame_j, n = 5, s = 1) 
p7 <- plot_bwpat(plot_frame_j, n = 4, s = 11) 
ggarrange(p6, p7, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(file.path(plot_path, "/Abb4.png"), width = 7.07, height = 5.38)

