Repository of data and code to reproduce analyses and figures for manuscript testing hypotheses in relation to patterns of carabid melanism using NEON pitfall specimen.

## data
Subdirectory of where the data for analysis is stored.
  - melanism.csv is a csv with melanism of all carabid beetles, including diurnal species.
  - melanism_modelDF is a csv used in analyses that is a filtered dataset of only nocturnal species. This csv also has columns for all model predictor variables.
  - neonBeetlePitfallData is a R data file of counts of number of carabids captured at NEON sites to calculate relative beetle abundance per site (relativeAbundanceBySite.csv)
### covariatesOutputs
Subdirectory containing calculated values of NEON site-level covariates included in our linear mixed models.  

## code
Subdirectory of scripts used to clean and wrangle data (dataWrangling) and to conduct analyses (analysis). All scripts in analysis can be run to fully reproduce the results and figures found in the associated manuscript. 
