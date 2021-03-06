# runcodemaster_cso011code_TanguroN2OLosses.R
# 
# disrupted N project
# CS O'Connell, UMN EEB/IonE
#
# master R code for Tanguro Greenhouse Gas paper
# see cso011w_TanguroN2OLosses for manuscript work
#


########################################################################
# PROCESS DATA

## Trace Gas Data

# process GC data (trace gas samples)
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/GC-Rcode-fileloop.R")
# convert that GC data into fluxes
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rcode/GC-Rcode-fileloop.R")

## Soil Inorganic N data

# process CENA data
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rcode/Process-soilNlabresults-Cleaning-Rcode.R")

# next step was I put that processed data into "Field-Extraction-Log/ExtractionLogCompiled/extractionlogcsv.csv" along with the MBL comparison data

# process bulk density data
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rcode/BulkDensityCalcs-Rcode.R")

# convert bulk density and extractionlogcsv.csv into inorganic N info
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rcode/ExtractionCalcs-Rcode.R")


########################################################################
# BUILD MASTER DATA SHEET

# build Tanguro-MasterDataSheet.csv
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Tanguro-MasterDataSheet.R")
# build Tanguro-MasterSiteDateSummary.csv
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Tanguro-MasterSiteDateSummary.r")


########################################################################
# EXPLORATORY FIGURES

# random exploratory figures go in TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet.R")


########################################################################
# EXPLORATORY STATS ANALYSES




########################################################################
# MANUSCRIPT FIGURES

# scatterplots of trace gas vs. moisture and soil N variables
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-Rcode/Figures-TanguroN2OLosses-scatterplotmatrix.r")

# scatterplots of trace gas vs. moisture and soil N variables
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Manuscript-Level-Figures-Tables-TanguroN2OLosses/Manuscript-Level-Figures-Tables-Rcode/Figures-TanguroN2OLosses-Sitelinegraphs.R")




########################################################################
# MANUSCRIPT ANALYSES






########################################################################
# NOTES AND TESTING



