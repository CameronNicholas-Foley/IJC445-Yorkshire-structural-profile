################################################################################
#Figure 1:
################################################################################

#Assigning region function
region_func <- function(itl_code) {
  case_when(grepl("^..C", itl_code) ~ "North East",
            grepl("^..D", itl_code) ~ "North West",
            grepl("^..E", itl_code) ~ "Yorkshire and The Humber",
            grepl("^..F", itl_code) ~ "East Midlands",
            grepl("^..G", itl_code) ~ "West Midlands",
            grepl("^..H", itl_code) ~ "East",
            grepl("^..I", itl_code) ~ "London",
            grepl("^..J", itl_code) ~ "South East",
            grepl("^..K", itl_code) ~ "South West",
            TRUE ~ itl_code)} 

#-------------------------------------------------------------------------------
#Collecting data and cleaning
#-------------------------------------------------------------------------------

#GDP
gdp_raw <- read_excel("raw_data/ONS_regional_economic_activity/regionalgrossdomesticproductgdpbyallinternationalterritoriallevelitlregions.xlsx", sheet = 9)

gdp_sliced <- gdp_raw%>%
  select(2, 3, 29)

colnames(gdp_sliced) <- c("itl_code", "local_authority", "gdp_millions")

#Removing Wales, Scotland, Northern Ireland
gdp_filtered <- gdp_sliced%>%
  filter(nchar(itl_code) == 5) #Filtering LAs only

gdp_filtered <- gdp_filtered[!grepl("^..[LMN]", gdp_filtered$itl_code), ] 

gdp_regions <- gdp_filtered%>%
  mutate(region = region_func(itl_code))

#GVA (File trimmed to required sheet only)
gva_raw <- read_excel("raw_data/ONS_regional_economic_activity/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions_trimmed.xlsx")%>%
  row_to_names(row_number = 1, remove_row = TRUE)

gva_sliced <- gva_raw %>% 
  select(1, 2, 3, 4, 30)

colnames(gva_sliced) <- c("itl_code", "local_authority", "industry_code", 
                          "industry", "gva_millions")

gva_filtered <- gva_sliced%>%
  filter(nchar(itl_code) == 5,
         industry_code == "Total")#Filtering LAs only

#Removing Wales, Scotland, Northern Ireland
gva_filtered <- gva_filtered[!grepl("^..[LMN]", gva_filtered$itl_code), ] 

gva_regions <- gva_filtered%>%
  mutate(region = region_func(itl_code))

gva_regions <- gva_regions%>%
  select(-c(industry_code, industry))

#GVA per hour worked
pro_raw <- read_excel("raw_data/ONS_subregional_productivity/labourproductivitylad1.xls", sheet = 3)%>%
  row_to_names(row_number = 4, remove_rows_above = TRUE)

#Using ITL lookup to merge with GDP and GVA data
pro_lookup <- read_csv("raw_data/ONS_subregional_productivity/LAD_(April_2025)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv") 

pro_raw <- pro_raw%>%
  left_join(pro_lookup, by = c("LAD_Name" = "LAD25NM"))

pro_sliced <- pro_raw%>%
  select(27, 28, 22)

colnames(pro_sliced) <- c("itl_code", "local_authority", "gva_per_hour_worked")

pro_sliced <- pro_sliced%>%
  mutate(gva_per_hour_worked = as.numeric(gva_per_hour_worked))

pro_filtered <- pro_sliced%>%
  filter(nchar(itl_code) == 5) #Filtering LAs only

#Removing Wales, Scotland, Northern Ireland
pro_filtered <- pro_filtered[!grepl("^..[LMN]", pro_filtered$itl_code), ] 

pro_agg <- pro_filtered%>%
  group_by(itl_code, local_authority)%>%
  summarise(across(everything(), ~ mean(.x)),
            .groups = "drop")

pro_regions <- pro_agg%>%
  mutate(region = region_func(itl_code))

#-------------------------------------------------------------------------------
#Merging Figure 1 Data
#-------------------------------------------------------------------------------

fig1_data <- gdp_regions%>%
  left_join(gva_regions, by = c("itl_code", "local_authority", "region"))%>%
  left_join(pro_regions, by = c("itl_code", "local_authority", "region"))

#Converting all values to correct data types
fig1_data <- fig1_data%>%
  mutate(across(-c(itl_code, local_authority, region), as.numeric))

#-------------------------------------------------------------------------------
#Transforming data for plotting
#-------------------------------------------------------------------------------

fig1_long <- fig1_data%>% #Pivoting to long format
  pivot_longer(cols = c(gdp_millions, gva_millions, gva_per_hour_worked),
               names_to = "metric",
               values_to = "value")%>%
  mutate(metric = recode(metric,
                         gdp_millions = "GDP (£ Millions)",
                         gva_millions = "GVA (£ Millions)",
                         gva_per_hour_worked = "GVA per Hour Worked (£) (Productivity)"))

england_avg <- fig1_long%>% #Computing metric averages
  group_by(metric)%>%
  summarise(england_avg = mean(value))

yh_avg <- fig1_long%>% #Extracting Yorkshire and the Humber
  filter(region %in% "Yorkshire and The Humber")%>%
  group_by(metric)%>%
  summarise(yh_avg = mean(value))

yh_label <- yh_avg%>%
  mutate(x = "", label = "Yorkshire and The Humber")

fig1_plot <- fig1_long%>% #Joining UK averages to fig1_long
  left_join(england_avg, by = "metric")%>%
  left_join(yh_avg, by = "metric")

fig1_plot$metric <- factor(fig1_plot$metric, levels = c(
  "GVA per Hour Worked (£) (Productivity)", "GVA (£ Millions)", "GDP (£ Millions)"))

#Figure 1: Yorkshire and The Humber's Lagging Productivity (2023)
fig1_boxplot <- ggplot(fig1_plot, aes(x = "", y = value))+
  geom_boxplot(width = 0.5, fill = "grey85", colour = "grey50", outlier.shape = NA)+
  geom_jitter(width = 0.08, alpha = 0.15, size = 1, colour = "grey45")+
  geom_hline(aes(yintercept = england_avg, linetype = "England average"),
             colour = "grey50", linewidth = 0.6)+
  geom_point(aes(y = yh_avg), colour = "#006EB6", shape = 18,size = 3.5)+
  geom_text_repel(data = yh_label, aes(x = x, y = yh_avg, label = label),
                  inherit.aes = FALSE, colour = "#006EB6", size = 3.2,
                  nudge_x = 1.9, direction = "y", hjust = 0,
                  segment.colour = "grey50", segment.size = 0.4, 
                  min.segment.length = 0)+
  
  facet_wrap(~ metric, scales = "free_y")+
  
  scale_linetype_manual(name = "", values = c("England average" = "dashed"))+
  scale_y_continuous(labels = scales::comma)+
  
  labs(title = "Figure 1: Yorkshire and The Humber's Lagging Productivity (2023)",
       subtitle = "Local Authority distributions shown with England and Yorkshire averages highlighted",
       x = NULL,
       y = "Local Authority Values",
       caption = "England average calculated as the mean of Local Authorities. Yorkshire and the Humber mean highlighted.\nSource: Office for National Statistics (2025).")+
  
  theme_few()+
  theme(panel.grid.major.y = element_line(colour = "grey85"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey30"),
        plot.caption = element_text(size = 9),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.key.size = unit(1, "cm",))

print(fig1_boxplot)
ggsave("figure1_boxplot.png", plot = fig1_boxplot, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)