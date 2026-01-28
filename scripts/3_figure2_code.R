################################################################################
#Figure 2:
################################################################################


#-------------------------------------------------------------------------------
#Collecting data and cleaning
#-------------------------------------------------------------------------------

#Business Regional Employment Survey (BRES) (Warning supressed as row 7 contained multiple "Flag" cols)
bres_raw <- suppressWarnings(read_excel("raw_data/bres/nomis_2025_12_18_150358.xlsx")%>%
                               row_to_names(row_number = 7, remove_rows_above = TRUE)%>%
                               select(-Flags)%>%
                               slice(-1)%>%
                               slice(-( (n()-1):n() ))%>%
                               rename("itl_code_name" =`international territorial levels - level 3 (as of Jan 2021)`)%>%
                               mutate(across(-c(itl_code_name), as.numeric)))

bres_filtered <- bres_raw%>%
  separate_wider_delim(cols = itl_code_name,
                       delim = " ",
                       names = c("itl_code", "local_authority"),
                       too_many = "merge")

bres_filtered <- bres_filtered%>%
  mutate(region = region_func(itl_code))

#Removing Wales, Scotland, Northern Ireland
bres_filtered <- bres_filtered[!grepl("^..[LMN]", bres_filtered$itl_code), ] 

bres_filtered <- bres_filtered%>%
  rename("T: Activities of households as employers" = `T : Activities of households as employers;undifferentiated goods-and services-producing activities of households for own use`)

#Pivoting regions to long format
bres_long <- bres_filtered%>%
  pivot_longer(cols = c(
    `A : Agriculture, forestry and fishing`:
      `U : Activities of extraterritorial organisations and bodies`),
    names_to = "industry",
    values_to = "employee_count")

#-------------------------------------------------------------------------------
#Harmonising LAs with previous datasets for accuracy
#-------------------------------------------------------------------------------

#Creating local authority crosswalk
la_harmonisation <- tribble(      
  ~local_authority,   ~la_final,
  
  "Camden", "Westminster, Camden and City of London",
  "Westminster", "Westminster, Camden and City of London",
  "Camden and City of London", "Westminster, Camden and City of London",
  "Westminster and City of London", "Westminster, Camden and City of London",
  
  "Barnsley", "Barnsley, Doncaster and Rotherham",
  "Doncaster", "Barnsley, Doncaster and Rotherham",
  "Rotherham", "Barnsley, Doncaster and Rotherham",
  
  "North Yorkshire", "North Yorkshire CC",
  
  "Berkshire East", "Berkshire",
  "Berkshire West", "Berkshire",
  
  "Babergh and Mid Suffolk", "Suffolk",
  "Ipswich", "Suffolk",
  "East Suffolk", "Suffolk",
  "West Suffolk", "Suffolk",
  
  "Somerset", "Somerset CC",
  "Bath & North East Somerset and South Gloucestershire", "Bath and North East Somerset, North Somerset and South Gloucestershire",
  "North Somerset", "Bath and North East Somerset, North Somerset and South Gloucestershire",
  
  "North and East Hertfordshire", "Herfordshire CC",
  "South West Hertfordshire", "Hertfordshire CC"
)

harmonisation_fun <- function(df) { #Function to apply harmonised LAs
  df %>%
    left_join(la_harmonisation, by = "local_authority") %>%
    mutate(la_final = coalesce(la_final, local_authority))
}

bres_harmonised <- harmonisation_fun(bres_long)


cumbria_las <- c( #Removing Cumbria LAs
  "West Cumbria",
  "East Cumbria",
  "Cumberland",
  "Westmorland and Furness"
)

fig2_data <- bres_harmonised %>%
  filter(!local_authority %in% cumbria_las)%>%
  group_by(la_final, industry, region) %>%
  summarise(employee_count = sum(employee_count, na.rm = TRUE), .groups = "drop")

colnames(fig2_data) <- c("local_authority", "industry", "region", 
                         "employee_count")

#-------------------------------------------------------------------------------
#Data transformations for plotting
#-------------------------------------------------------------------------------

#Aggregating employment count for Yorkshire and The Humber
yh_emp <- fig2_data%>%
  filter(region %in% "Yorkshire and The Humber")%>%
  group_by(region, industry)%>%
  summarise(employee_count = sum(employee_count, na.rm = TRUE),
            .groups = "drop")

#Aggregating employment count for whole dataset (England)
england_emp <- fig2_data%>%
  group_by(industry)%>%
  summarise(employee_count = sum(employee_count, na.rm = TRUE),
            .groups = "drop")%>%
  mutate(region = "England")

emp_share <- yh_emp%>% #Computing employment shares
  bind_rows(england_emp)%>%
  group_by(region)%>%
  mutate(emp_share = employee_count / sum(employee_count))%>%
  ungroup()

emp_deviation <- emp_share%>% #Computing deviation from share
  select(region, industry, emp_share)%>%
  pivot_wider(names_from = region,
              values_from = emp_share)%>%  
  mutate(deviation = `Yorkshire and The Humber` - England)

fig2_plot <- emp_deviation%>% #Ordering for plotting
  mutate(industry = factor(industry, levels = industry[order(deviation)]))

#Figure 2: Yorkshire and The Humber's Deviating Industrial Employment Profile (2023
fig2_diverg <- ggplot(fig2_plot, aes(x = industry, y = deviation, fill = deviation > 0))+
  geom_col(width = 0.75, show.legend = FALSE)+
  coord_flip()+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7,
             colour = "grey80")+
  
  scale_fill_manual(values = c("TRUE" = "#5EC962",
                               "FALSE" = "#3B528B"))+
  
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0.05, 0.05)))+
  
  labs(title = "Figure 2: Yorkshire and The Humber's Deviating Industrial Employment Profile (2023)",
       subtitle = "Positive values indicate industries over-represented in Yorkshire relative to England",
       x = NULL,
       y = "Deviation From England Employment Share (%)",
       caption = "Deviation equals Yorkshire and The Humber share minus mean England share.\nSource: Office for National Statistics (2024).")+
  
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 12, face = "bold",
                                    margin = margin(t = 10)),
        plot.title = element_text(size = 13, hjust = 1, face = "bold"),
        plot.subtitle = element_text(size = 11, hjust = 4.5,  colour = "grey30"),
        plot.caption = element_text(size = 9, margin = margin(t = 20),
                                    hjust = 1),
        plot.margin = margin(t = 20, r = 15, b = 15, l = 10))


print(fig2_diverg)
ggsave("figure2_diverging_bar_char.png", plot = fig2_diverg, path = "outputs", 
       height = 783, width = 955, units = "px", dpi = 96)
