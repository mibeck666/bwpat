#Plots with sjPlot, colors: https://twitter.com/dougmcneall/status/1002566525868527617

#Prepare for plotting: Renaming Variables

plot_df_wert <- full_data %>%
  select(Studie, starts_with("wert")) %>%
  mutate_at(vars(starts_with("wert")), ~ factor(., labels = c("vˆllig unwichtig",
                                                               "eher unwichtig",
                                                               "eher wichtig",
                                                               "sehr wichtig"))) %>% 

  rename('Gesundheitsbewusstsein' = wert_1,
         'Leben geniessen' = wert_2,
         'Umweltbewusststein' = wert_3,
         'Gute Ausbildung' = wert_23,
         'Gute Beziehungen' = wert_24,
         'Macht und Einfluss' = wert_4,
         'Gesetz und Ordnung' = wert_5,
         'Sicherheit' = wert_6,
         'Fleiﬂ und Ehrgeiz' = wert_7,
         'Hoher Lebensstandard' = wert_8,
         'Durchsetzungskraft' = wert_9,
         'Phantasie und Kreativit‰t' = wert_10,
         'Sozial Benachteiligten und gesellschaftlichen Randgruppen helfen' = wert_11,
         'Abweichende Meinungen tolerieren' = wert_12,
         'Politisches Engagement' = wert_13,
         'Unabh‰ngigkeit' = wert_14,
         'Abwechslungsreichtum und Aufregung' = wert_25,
         'An Traditionen halten' = wert_15,
         'Tun, was andere tun' = wert_16,
         'Eigenverantwortlich leben und handeln' = wert_17)

plot_df_ber <- full_data %>%
  select(Studie,
         starts_with("ber"),
         ) %>%
  mutate_at(vars(starts_with("ber")), ~ factor(., labels = c("vˆllig unwichtig",
                                                              "eher unwichtig",
                                                              "eher wichtig",
                                                              "sehr wichtig"))) %>% 
  
  rename('Spaﬂ' = berpraef_1,
         'Geld' = berpraef_17,
         'Verantwortung' = berpraef_2,
         'Prestige' = berpraef_13,
         'Arbeitsplatzsicherheit' = berpraef_7,
         'Karriere' = berpraef_4,
         'Zeit f¸r Hobbies' = berpraef_3,
         'Sinnvolles f¸r Gesellschaft' = berpraef_12,
         'Zeit f¸r Familie' = berpraef_9)

#LeOJ Values
plot_df_wert %>% filter(Studie == "Sch¸ler*innen Oberstufe") %>% 
  select(-Studie) %>% 
  plot_likert(grid.range = c(1.2, 1.4),
              expand.grid = FALSE,
              values = "sum.outside",
              show.prc.sign = TRUE,
              legend.pos = "bottom",
              sort.frq = "pos.asc",
              geom.colors = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
              reverse.scale = TRUE) +
  theme_minimal() + 
  theme(legend.position = "bottom")

  ggsave(file.path(plot_path, "/Abb1a.png"), width = 8, height = 8)
  
  #LeOJ Job Goals
plot_df_ber %>% filter(Studie == "Sch¸ler*innen Oberstufe") %>% 
 select(-Studie) %>% 
 plot_likert(grid.range = c(1.2, 1.4),
             expand.grid = FALSE,
             values = "sum.outside",
             show.prc.sign = TRUE,
             legend.pos = "bottom",
             sort.frq = "pos.asc",
             geom.colors = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
             reverse.scale = TRUE) +
 theme_minimal() + 
 theme(legend.position = "bottom")
  
  ggsave(file.path(plot_path, "/Abb1b.png"), width = 8, height = 3.81)

#LEBEL Values
  plot_df_wert %>% filter(Studie == "Auszubildende") %>% 
    select(-Studie) %>% 
    plot_likert(grid.range = c(1.2, 1.4),
                expand.grid = FALSE,
                values = "sum.outside",
                show.prc.sign = TRUE,
                legend.pos = "bottom",
                sort.frq = "pos.asc",
                geom.colors = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
                reverse.scale = TRUE) +
    theme_minimal() + 
    theme(legend.position = "bottom")
  
  ggsave(file.path(plot_path, "/Abb2a.png"), width = 8, height = 8)

#LEBEL Job
plot_df_ber %>% filter(Studie == "Auszubildende") %>% 
  select(-Studie) %>% 
  plot_likert(grid.range = c(1.2, 1.4),
              expand.grid = FALSE,
              values = "sum.outside",
              show.prc.sign = TRUE,
              legend.pos = "bottom",
              sort.frq = "pos.asc",
              geom.colors = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
              reverse.scale = TRUE) +
  theme_minimal() + 
  theme(legend.position = "bottom")
  
  ggsave(file.path(plot_path, "/Abb2b.png"), width = 8, height = 3.81)

