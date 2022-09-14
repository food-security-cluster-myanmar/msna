
mini %>% 
  ggplot(aes(x = csi_unweighted)) + 
  geom_histogram(bins = 30) + 
  geom_freqpoly(binwidth = 1, 
                colour = "red",
                alpha = .7) + 
  
  
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Histograms of coping strategies index scores", 
                  subtitle = "IFPRI 2022")

fct_relevel(word, 
            c("threatenedviolence","robbed", "none",
              "detained", "sexualharassment", "kidnapped",
              "recruitedarmedgroups", "injuredexplosive", "trafficked",
              "killed", "harrassmentnotsexual", "verbalharassment",
              "discrimination", "earlymarriage", "dontknow",
              "sentabroad", "forciblymarried", "other",
              "exploited", "fmg", "prefernottoanswer"))

msna %>%
  mutate(not_improved_drinking_water = 
           case_when(wash_drinkingwatersource_dry %in% c(
             "borewhole_tubewell", 
             "piped_connection_to_house_or_neighbors_house", 
             "protected_spring", 
             "protected_well", 
             "public_tapstandpipe", 
             "rain_water_collection"
           ) ~ 0, 
           TRUE ~ 1)) %>% 
  mutate()
count(wash_drinkingwatersource_dry)

msna %>% 
  filter(hh_number_girls_12_14_count > 0 | hh_number_boys_12_14_count > 0) %>%  
  group_by(state = state_region_2) %>% 
  summarise(girls_total = sum(hh_number_girls_12_14_count),
            girls_enrolled = sum(edu_enrol_girl_sec1, na.rm = TRUE), 
            boys_total = sum(hh_number_boys_12_14_count), 
            boys_enrolled = sum(edu_enrol_boy_sec1, na.rm = TRUE)) %>% 
  mutate(girls_12_14 = round(girls_enrolled / girls_total * 100, digits = 2), 
         boys_12_14 = round(boys_enrolled / boys_total * 100, digits = 2)) %>% 
  pivot_longer(cols = c(girls_12_14, boys_12_14), 
               names_to = "var", 
               values_to = "value") %>% 
  ggplot(aes(x = value, y = reorder_within(state, value, var))) + 
  geom_col(aes(fill = state), 
           alpha = .8) +
  geom_text(aes(label = value),
            colour = "grey20", 
            size = 3, 
            hjust = "inward") + 
  scale_y_reordered() + 
  facet_wrap(~ var, scales = "free_y") + 
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "#212121")) + 
  labs(x = "Percent enrolled", 
       y = "", 
       title = "States by primary school enrollment (lower secondary)", 
       subtitle = "MSNA", 
       caption = "REACH 2022")

msna %>% 
  filter(hh_number_girls_15_17_count > 0 | hh_number_boys_15_17_count > 0) %>%  
  group_by(state = state_region_2) %>% 
  summarise(girls_total = sum(hh_number_girls_15_17_count),
            girls_enrolled = sum(edu_enrol_girl_sec2, na.rm = TRUE), 
            boys_total = sum(hh_number_boys_15_17_count), 
            boys_enrolled = sum(edu_enrol_boy_sec2, na.rm = TRUE)) %>% 
  mutate(girls_15_17 = round(girls_enrolled / girls_total * 100, digits = 2), 
         boys_15_17 = round(boys_enrolled / boys_total * 100, digits = 2)) %>% 
  pivot_longer(cols = c(girls_15_17, boys_15_17), 
               names_to = "var", 
               values_to = "value") %>% 
  ggplot(aes(x = value, y = reorder_within(state, value, var))) + 
  geom_col(aes(fill = state), 
           alpha = .8) +
  geom_text(aes(label = value),
            colour = "grey20", 
            size = 3, 
            hjust = "inward") + 
  scale_y_reordered() + 
  facet_wrap(~ var, scales = "free_y") + 
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "#212121")) + 
  labs(x = "Percent enrolled", 
       y = "", 
       title = "States by primary school enrollment (lower secondary)", 
       subtitle = "MSNA", 
       caption = "REACH 2022")