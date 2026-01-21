# IJC445-Yorkshire-structural-profile
Analysing Yorkshire and The Humber's Economic, Industrial, and Human Capital Profile Relative to English LAs and Its Associations to Labour Productivity | IJC445 Data Visualisation Coursework.

Overview
-
Labour productivity is a key economic indicator, reflecting workforce resilience, efficiency, and regional competitiveness.  Productivity outcomes are shaped 
by complex structural differences, including spatial economic characteristics, industrial composition, and human capital. This project studies Yorkshire and The Humber, 
a region with a strong manufacturing history and local authority-level heterogeneity. 

Using data from the Office for National Statistics and the Department for Education, a composite visualisation consisting of four charts was developed to examine how the region 
compares against English local authorities across the key structural characteristics, and how they spatially interact.

Research Question
-
How do economic, industrial, and human capital characteristics structurally position Yorkshire and The Humber relative to labour productivity, 
and how distinct is this from England as a whole?

Key Findings
-
- Relative to English local authorities, Yorkshire and The Humber performs notably lower in Gross Value Added (GVA) per hour worked (productivity indicator) compared to other economic indicators.
- Yorkshire and The Humber remains overrepresented in manufacturing employment relative to the English local authority average, while, underrepresented in knowledge-intensive industries.
- Across educational and health outcomes, Yorkshire and The Humber is consistently positioned toward the lower-end of national local authority distributions.
- Summarising all of the preceding data, Principal Component Analysis (PCA) was able to explain 43.8% of the variance. Yorkshire and The Humber formed a tight cluster characterised by manufacturing and trade services. These characteristics position with lower economic, educational and health outcomes relative to the national distribution.

Code
-
- All data inputs are stored in the `raw_data/` directory (trimmed data sets to reduce file size are specified and commented).
- All R scripts are in the `scripts/` directory and are organised to reflect incremental knowledge building:
  -  Library loading.
  -  Figure 1: Boxplot of economic indicators.
  -  Figure 2: Diverging bar chart of industrial composition relative to English local authority average.
  -  Figure 3: Parallel coordinates plot of national local authorities across human capital indicators with Yorkshire and The Humber represented as median line.
  -  Figure 4: PCA biplot of the data visualised in the preceding figures (excluding GVA per hour worked).
  -  A composite outputs shortcut script.
- Figures are saved automatically in the `outputs/` directory once the code is executed.

Instructions to Run Code
-
1. Clone the repository
```bash
https://github.com/CameronNicholas-Foley/IJC445-Yorkshire-structural-profile.git
```
2. Open RStudio, set the cloned repository as the repository URL (File -> New Project -> Version Control -> Git).
3. Run `1_libraries_IJC445.R` and install the packages if required.
4. Execute the remaining scripts in the numerical order as labelled.
5. Outputs will be saved in the `outputs/` directory.

IJC437 Project Link
-
The associated IJC437 project coursework and outputs are available here: https://github.com/CameronNicholas-Foley/IJC437-digital-employment-growth
