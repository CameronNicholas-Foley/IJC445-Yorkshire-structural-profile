################################################################################
#Figure 3:
################################################################################


#-------------------------------------------------------------------------------
#Collecting data and cleaning
#-------------------------------------------------------------------------------

#GCSE 9-4 pass pct (Blank spaces and "z" treated as NA in excel file) (File trimmed to required year only)
gcse_raw <- read_excel("raw_data/dfe/202425_local_authority_district_provisional_trimmed.xlsx",
                       na = c("", "NA", "z"))

gcse_filtered <- gcse_raw%>%
  filter(country_name %in% "England",
         geographic_level %in% "Local authority district",
         version %in% "Final")%>%
  select(time_period, region_name, lad_name, engmath_94_percent,
         ebacc_entering_percent)

colnames(gcse_filtered) <- c("time_period", "region", "local_authority", 
                             "engmath_94_percent", "ebacc_entering_percent")

gcse_transformed <- gcse_filtered%>%
  mutate(region = case_when(
    region %in% c("Inner London", "London", "Outer London") ~ "London",
    TRUE ~ region))

gcse_cleaned <- gcse_transformed%>%
  group_by(region, local_authority)%>%
  summarise(avg_engmath_94_pct = mean(engmath_94_percent),
            avg_ebacc_entry_pct = mean(ebacc_entering_percent))

#Level 3 attainment pct (Blank spaces and "z" treated as NA in excel file) (File trimmed to required year only)
lvl3_raw <- read_excel("raw_data/dfe/level_2_3_ages_16_19_local_authority_figures_trimmed.xlsx",
                       na = c("", "NA", "z"))

#Isles of Scilly has NA values
lvl3_filtered <- lvl3_raw%>%
  filter(geographic_level %in% "Local authority",
         country_name %in% "England",
         qualification_level %in% "Level 3")%>%
  select(time_period, la_name, qualification_level, percentage)%>%
  mutate(percentage = as.numeric(percentage))%>%
  drop_na()

colnames(lvl3_filtered) <- c("time_period", "local_authority", 
                             "qualification_level", "percentage")

lvl3_cleaned <- lvl3_filtered%>%
  group_by(local_authority)%>%
  summarise(level_3_attainment_pct = mean(percentage))

#Male and female average life expectancy from <1 (File trimmed to required sheet and year only)
life_expec_raw <- read_excel("raw_data/ONS_life_expectancy/lifeexpectancylocalareas_trimmed.xlsx")%>%
  row_to_names(row_number = 5, remove_rows_above = TRUE)

life_expec_filtered <- life_expec_raw%>%
  filter(Country %in% "England",
         `Area type` %in% "Local Areas",
         `Age group` %in% "<1")%>%
  select(`Area name`, Sex, `Life expectancy`)%>%
  mutate(across(-c(`Area name`, Sex), as.numeric))

colnames(life_expec_filtered) <- c("local_authority", "sex", "life_expectancy")

life_expec_cleaned <- life_expec_filtered%>%
  pivot_wider(names_from = sex,
              values_from = life_expectancy)

colnames(life_expec_cleaned) <- c("local_authority", "male_life_expectancy", 
                                  "female_life_expectancy")

#Male and female average healthy life expectancy from <1 (File trimmed to required sheet only)
healthy_life_expec_raw <- read_excel("raw_data/ONS_life_expectancy/healthylifeexpectancyenglandandwales_trimmed.xlsx")%>%
  row_to_names(row_number = 5, remove_rows_above = TRUE)

healthy_life_expec_filtered <- healthy_life_expec_raw%>%
  filter(Period %in% "2021 to 2023", 
         Country %in% "England",
         `Area type` %in% "Local Areas",
         `Age group` %in% "<1")%>%
  select(`Area name`, Sex, `Healthy life expectancy`)%>%
  mutate(across(-c(`Area name`, Sex), as.numeric))


colnames(healthy_life_expec_filtered) <- c("local_authority", "sex", 
                                           "healthy_life_expectancy")

healthy_life_expec_cleaned <- healthy_life_expec_filtered%>%
  pivot_wider(names_from = sex,
              values_from = healthy_life_expectancy)

colnames(healthy_life_expec_cleaned) <- c("local_authority", 
                                          "male_healthy_life_expectancy", 
                                          "female_healthy_life_expectancy")

#-------------------------------------------------------------------------------
#Merging Figure 3 data
#-------------------------------------------------------------------------------

fig3_data <- healthy_life_expec_cleaned%>%
  left_join(gcse_cleaned, by = "local_authority")%>%
  left_join(life_expec_cleaned, by = "local_authority")%>%
  left_join(lvl3_cleaned, by = "local_authority")%>%
  select(region, local_authority, avg_engmath_94_pct, avg_ebacc_entry_pct, 
         level_3_attainment_pct, male_life_expectancy, female_life_expectancy,
         male_healthy_life_expectancy, female_healthy_life_expectancy)%>%
  drop_na() #Cumberland and Westmorland and Furness removed

fig3_data <- fig3_data%>%
  rename("English/Maths 9-4" = avg_engmath_94_pct,
         "EBacc entry rate" = avg_ebacc_entry_pct,
         "Level 3 education attainment" = level_3_attainment_pct,
         "Male life expectancy" = male_life_expectancy,
         "Female life expectancy" = female_life_expectancy,
         "Male healthy life expectancy" = male_healthy_life_expectancy,
         "Female healthy life expectancy" = female_healthy_life_expectancy)

#-------------------------------------------------------------------------------
#Transforming data for plotting
#-------------------------------------------------------------------------------

fig3_plot <- fig3_data%>%
  mutate(la_groups = ifelse(region == "Yorkshire and The Humber", 
                            "Yorkshire and The Humber LAs", "English LAs"))

fig3_plot <- fig3_plot%>%
  pivot_longer(cols = 3:9, names_to = "indicator", values_to = "value")

#Ordering education indicators, health indicators
indicator_order <- c("English/Maths 9-4",
                     "EBacc entry rate",
                     "Level 3 education attainment",
                     "Male life expectancy",
                     "Female life expectancy",
                     "Male healthy life expectancy",
                     "Female healthy life expectancy")

fig3_plot <-fig3_plot%>%
  mutate(indicator = factor(indicator, levels = indicator_order))

fig3_plot <- fig3_plot %>%
  mutate(domain = case_when(
    indicator %in% c(
      "English/Maths 9-4",
      "EBacc entry rate",
      "Level 3 education attainment") ~ "Educational attainment (%)",
    indicator %in% c(
      "Male life expectancy",
      "Female life expectancy",
      "Male healthy life expectancy",
      "Female healthy life expectancy") ~ "Health outcomes (Years)"))

yh_summary3 <- fig3_plot%>%
  filter(region == "Yorkshire and The Humber")%>%
  group_by(domain, indicator)%>%
  summarise(median = median(value, na.rm = TRUE),
            q25 = quantile(value, 0.25, na.rm = TRUE),
            q75 = quantile(value, 0.75, na.rm = TRUE),
            .groups = "drop")

#Line to differentiate indicators
indicator_levels <- levels(factor(fig3_plot$indicator)) 

#Figure 3: Yorkshire and The Humber's Human Capital Relative to English Local Authorities (2021-2023)
fig3_pcp <- ggplot()+
  geom_line(data = fig3_plot%>%
              filter(region != "Yorkshire and The Humber"),
            aes(x = indicator, y = value, group = local_authority),
            colour = "grey77", alpha = 0.3)+
  
  geom_ribbon(data = yh_summary3, aes(x = indicator, ymin = q25, ymax = q75,
                                      group = 1),
              fill = "#006EB6", alpha = 0.25)+
  
  geom_line(data = yh_summary3, aes(x = indicator, y = median, group = 1),
            colour = "#006EB6", linewidth = 1.3)+
  
  geom_point(data = yh_summary3, aes(x = indicator, y = median, group =1),
             colour = "#006EB6", size = 2)+
  
  geom_vline(data = fig3_plot%>%
               distinct(domain, indicator)%>%
               group_by(domain)%>%
               mutate(x_id = row_number()),
             aes(xintercept = x_id),
             colour = "grey70", linewidth = 0.4, linetype = "dotted")+
  
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     expand = expansion(mult = 0))+
  
  scale_x_discrete(expand = expansion(mult = 0.2))+
  
  facet_wrap(~domain, scales = "free_x", nrow = 1)+
  
  labs(title = "Figure 3: Yorkshire and The Humber's Human Capital Relative to English Local Authorities (2021-2023)",
       subtitle = "Parallel coordinate plot highlighting Yorkshire and The Humber against national distribution of human capital indicators",
       y = "Indicator Value (Original Scale)",
       x = NULL,
       caption = "Grey lines represent English local authorities; blue band shows interquartile range for Yorkshire and The Humber
       Source: Department Deparment for Education (2025); Office for National Statistics (2024-2025).")+
  
  theme_few()+
  theme(plot.margin = margin (8, 5, 8, 5),
        axis.text.x = element_text(angle = 35, size = 10, face = "bold",
                                   hjust = 1),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey30"),
        plot.caption = element_text(size = 9, margin = margin(t = 30)))

print(fig3_pcp)
ggsave("figure3_pcp.png", plot = fig3_pcp, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)