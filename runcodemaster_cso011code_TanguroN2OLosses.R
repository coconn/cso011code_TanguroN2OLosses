# runcodemaster_cso011code_TanguroN2OLosses.R
# 
# disrupted N project
# CS O'Connell, UMN EEB/IonE
#
# master R code for Tanguro Greenhouse Gas paper
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
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet.R")


########################################################################
# EXPLORATORY FIGURES

# random exploratory figures go in TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet/
source("~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Analysis-Figures-Tanguro-MasterDataSheet.R")


########################################################################
# EXPLORATORY STATS ANALYSES




########################################################################
# MANUSCRIPT FIGURES




########################################################################
# MANUSCRIPT ANALYSES




########################################################################
# NOTES AND TESTING



