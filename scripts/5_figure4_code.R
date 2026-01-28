################################################################################
#Figure 4
################################################################################


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
fig4_harmon <- tribble(  
  ~local_authority, ~la_final,
  
  #London
  "Camden", "Westminster, Camden and City of London",
  "Westminster", "Westminster, Camden and City of London",
  "City of London", "Westminster, Camden and City of London",
  
  #Yorkshire
  "Barnsley", "Barnsley, Doncaster and Rotherham",
  "Doncaster", "Barnsley, Doncaster and Rotherham",
  "Rotherham", "Barnsley, Doncaster and Rotherham",
  
  "North Yorkshire", "North Yorkshire CC",
  
  #Berkshire
  "Berkshire East", "Berkshire",
  "Berkshire West", "Berkshire",
  
  #Suffolk
  "Babergh and Mid Suffolk", "Suffolk",
  "Ipswich", "Suffolk",
  "East Suffolk", "Suffolk",
  "West Suffolk", "Suffolk",
  
  #Somerset
  "Somerset", "Somerset CC",
  "Bath and North East Somerset", "Bath and North East Somerset, North Somerset and South Gloucestershire",
  "North Somerset", "Bath and North East Somerset, North Somerset and South Gloucestershire",
  "South Gloucestershire", "Bath and North East Somerset, North Somerset and South Gloucestershire",
  
  #Hertfordshire
  "North and East Hertfordshire", "Hertfordshire",
  "South West Hertfordshire", "Hertfordshire")

fig4_harmon_fun <- function(df) {
  df%>%
    left_join(fig4_harmon, by = "local_authority")%>%
    mutate(la_final = coalesce(la_final, local_authority))
}

#-------------------------------------------------------------------------------
#Applying harmonisation function to all data
#-------------------------------------------------------------------------------

fig1_clean <- fig1_data%>%
  fig4_harmon_fun()%>%
  filter(!local_authority %in% c("Cumberland", "Westmorland and Furness"))%>%
  group_by(la_final, region)%>%
  summarise(gdp_millions = sum(gdp_millions, na.rm = TRUE), 
            gva_millions = sum(gva_millions, na.rm = TRUE),
            .groups = "drop")%>%
  rename("GDP (£ Millions)" = gdp_millions,
         "GVA (£ Millions)" = gva_millions)

fig2_clean <- fig2_data%>%
  fig4_harmon_fun()%>%
  filter(!local_authority %in% "Cornwall and Isles of Scilly")%>%
  group_by(la_final, industry)%>%
  summarise(employee_count = sum(employee_count, na.rm = TRUE),
            .groups = "drop")%>%
  group_by(la_final)%>%
  mutate(industry_share = employee_count / sum(employee_count, na.rm = TRUE))%>%
  ungroup()

fig2_clean <- fig2_clean%>%
  select(la_final, industry, industry_share)%>%
  pivot_wider(names_from = industry,
              values_from = industry_share)

#Removing industries with multiple 0 values
fig2_clean <- fig2_clean%>%
  select(-c(`T: Activities of households as employers`, `U : Activities of extraterritorial organisations and bodies`))

fig3_clean <- fig3_data%>%
  fig4_harmon_fun()%>%
  filter(!local_authority %in% "Isles of Scilly")%>%
  group_by(la_final)%>%
  summarise(`English/Maths 9-4` = mean(`English/Maths 9-4`, na.rm = TRUE),
            `EBacc entry rate` = mean(`EBacc entry rate`, na.rm = TRUE),
            `Level 3 education attainment` = mean(`Level 3 education attainment`, na.rm = TRUE),
            `Male life expectancy` = mean(`Male life expectancy`, na.rm = TRUE),
            `Female life expectancy` = mean(`Female life expectancy`, na.rm = TRUE),
            `Male healthy life expectancy` = mean(`Male healthy life expectancy`, na.rm = TRUE),
            `Female healthy life expectancy` = mean(`Female healthy life expectancy`, na.rm = TRUE),
            .groups = "drop")

#-----------------------------------------------------------------------------------------------------------------
#Merging Figure 4 data
#-----------------------------------------------------------------------------------------------------------------

fig4_data <- fig1_clean%>%
  left_join(fig2_clean, by = "la_final")%>%
  left_join(fig3_clean, by = "la_final")

fig4_data <- fig4_data%>%
  drop_na()

#-----------------------------------------------------------------------------------------------------------------
#Preparing PCA matrix
#-----------------------------------------------------------------------------------------------------------------

fig4_matrix <- fig4_data%>%
  select(-la_final, -region)

fig4_pca_model <- prcomp(fig4_matrix, scale. = TRUE)

#-----------------------------------------------------------------------------------------------------------------
#Plotting PCA
#-----------------------------------------------------------------------------------------------------------------

#Figure 4: Structural Positioning of Yorkshire and The Humber Vs English Local Authorities (2021-2023)
#NOTE: ggplot2 warning may appear regarding deprecated use of 'size' for lines
#This does not affect the visualisation output.
fig4_pca <- fviz_pca_biplot(fig4_pca_model, geom.ind ="point", pointshape = 21,
                            fill.ind = ifelse(fig4_data$region == "Yorkshire and The Humber", "Yorkshire and The Humber", 
                                              "English Local Authorities"),
                            col.ind = "black", pointsize = 3, alpha.ind = 0.8,
                            
                            label = "var", col.var = "black", repel = TRUE, 
                            select.var = list(contrib = 15),
                            arrowsize = 0.2, labelsize = 3,
                            
                            addEllipses = TRUE, ellipse.level = 0.68)+
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70")+
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70")+
  
  labs(title = "Figure 4: Structural Positioning of Yorkshire and The Humber Vs English Local Authorities (2021-2023)",
       subtitle = "PCA combining economic scale, industrial composition, and human capital indicators",
       caption = "Variables scaled; zero-variance industries and GVA per hours worked (outcome) excluded.")+
  
  theme_few()+
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey30"),
        plot.caption = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_blank())

print(fig4_pca)
ggsave("figure4_pca.png", plot = fig4_pca, path = "outputs", 

       height = 783, width = 955, units = "px", dpi = 96)
