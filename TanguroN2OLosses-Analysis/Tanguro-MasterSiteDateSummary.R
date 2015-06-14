# Tanguro-MasterSiteDateSummary.R
# get summary stats for each site at each sampling date from Tanguro-MasterDataSheet.csv
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# reproduceable after any input csv files change
# see also Tanguro-MasterDataSheet.R

# output product:
# Tanguro-MasterSiteDateSummary.csv


########################################################################
# BRING IN DATA / PREP

fluxesfullmerge <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Tanguro-MasterDataSheet.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)

fluxesfullmerge$SampleDate2 <- as.Date(fluxesfullmerge$SampleDate2)

# prep table that summarizes by site and date (combines info from multiple chambers)

# data.table
dtfluxes <- data.table(fluxesfullmerge)
# create site-date summary table
sitedatesummary <- dtfluxes[,list(
      SampleDate2=unique(SampleDate2), 
      Site=unique(Site), 
      color.use=unique(color.use), 
      LUname=unique(LUname),
      meanfluxN2Ol=mean(na.omit(LinearFlux[GasType=="N2O"])),
      meanfluxCO2l=mean(na.omit(LinearFlux[GasType=="CO2"])),
      meanfluxCH4l=mean(na.omit(LinearFlux[GasType=="CH4"])),
      sdfluxN2Ol=sd(na.omit(LinearFlux[GasType=="N2O"])),
      sdfluxCO2l=sd(na.omit(LinearFlux[GasType=="CO2"])),  
      sdfluxCH4l=sd(na.omit(LinearFlux[GasType=="CH4"])),  
      meanfluxN2Oq=mean(na.omit(QuadFlux[GasType=="N2O"])),
      meanfluxCO2q=mean(na.omit(QuadFlux[GasType=="CO2"])),
      meanfluxCH4q=mean(na.omit(QuadFlux[GasType=="CH4"])),
      sdfluxN2Oq=sd(na.omit(QuadFlux[GasType=="N2O"])),
      sdfluxCO2q=sd(na.omit(QuadFlux[GasType=="CO2"])),
      sdfluxCH4q=sd(na.omit(QuadFlux[GasType=="CH4"])),
      meanSoilMoisPercent=mean(na.omit(SoilMoisPercent)),
      sdSoilMoisPercent=sd(na.omit(SoilMoisPercent)),
      meanNO3_N_mgNg=mean(na.omit(NO3_N_mgNg)),
      sdNO3_N_mgNg=sd(na.omit(NO3_N_mgNg)),
      meanNH4_N_mgNg=mean(na.omit(NH4_N_mgNg)),
      sdNH4_N_mgNg=sd(na.omit(NH4_N_mgNg)),
      meanNO3_N_NH4_N_mgNg=mean(na.omit(NO3_N_NH4_N_mgNg)),
      sdNO3_N_NH4_N_mgNg=sd(na.omit(NO3_N_NH4_N_mgNg)),
      meanNO3_N_mgNg_FinalMinusInitial_perDay=mean(na.omit(NO3_N_mgNg_FinalMinusInitial_perDay)),
      sdNO3_N_mgNg_FinalMinusInitial_perDay=sd(na.omit(NO3_N_mgNg_FinalMinusInitial_perDay)),
      meanNH4_N_mgNg_FinalMinusInitial_perDay=mean(na.omit(NH4_N_mgNg_FinalMinusInitial_perDay)),
      sdNH4_N_mgNg_FinalMinusInitial_perDay=sd(na.omit(NH4_N_mgNg_FinalMinusInitial_perDay)),
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay=mean(na.omit(NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay)),
      sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay=sd(na.omit(NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay)),
      meanNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis=mean(na.omit(NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis)),
      sdNO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis=sd(na.omit(NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis)),
      meanNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis=mean(na.omit(NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis)),
      sdNH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis=sd(na.omit(NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis)),
      meanNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis=mean(na.omit(NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis)),
      sdNO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis=sd(na.omit(NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis))),
      by=easysitename] # include as many variables as desired

# include standard error and CI as per http://www.cookbook-r.com/Manipulating_data/Summarizing_data/ ?


########################################################################
# SAVE Tanguro-MasterSiteDateSummary.csv

# save as csv
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/"
write.csv(sitedatesummary, file=paste(pathsavefiles, "Tanguro-MasterSiteDateSummary.csv", sep = ""), row.names=FALSE)  



########################################################################
# NOTES AND TESTING

# should I also include the overall site information (totC, totN, BD, etc. as columns here?)
# there's currently no leakage test analysis inclusion


########################################################################
# POSSIBLE TO DO





