################################################################################
#Composite outputs shortcut
################################################################################


source("1_libraries_IJC445.R")
source("2_figure1_code.R")
source("3_figure2_code.R")
source("4_figure3_code.R")
source("5_figure4_code.R")

#Figure 1: Yorkshire and The Humber's Lagging Productivity (2023)
print(fig1_boxplot)
ggsave("figure1_boxplot.png", plot = fig1_boxplot, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)

#Figure 2: Yorkshire and The Humber's Deviating Industrial Employment Profile (2023
print(fig2_diverg)
ggsave("figure2_diverging_bar_char.png", plot = fig2_diverg, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)

#Figure 3: Yorkshire and The Humber's Human Capital Relative to English Local Authorities (2021-2023)
print(fig3_pcp)
ggsave("figure3_pcp.png", plot = fig3_pcp, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)

#Figure 4: Structural Positioning of Yorkshire and The Humber Vs English Local Authorities (2021-2023)
print(fig4_pca)
ggsave("figure4_pca.png", plot = fig4_pca, path = "outputs", 
       height = 783, width = 978, units = "px", dpi = 96)