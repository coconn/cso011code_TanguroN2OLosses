# ExtractionCalcs-Rcode.R
# taking vial data and converting it into fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE
#
# bring in extractionlogcsv.csv 
# (which was compiled by hand based on soilNlabresults.csv, created in Process-soilNlabresults-Cleaning-Rcode.R)
# 
# take that soil N extraction data and calculate:
# [NO3], [NH4], [NO3+NH4]
# net N min, net N nitr, net immobilization (per gram basis and per area basis)
#
# see Process-soilNlabresults-Cleaning-Rcode.R, BulkDensityCalcs-Rcode.R
#
# output products:
# extractionDF_processed.csv: reports NO3 and NH4 as mg N/g soil and not ppm
# 
# this information is then included in Soil-Data-RawFolders/abioticfactors.txt by hand
# then re-run Tanguro-MasterDataSheet.R
# 



########################################################################
# BRING IN DATA, ETC.

library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)
library(lubridate)

# bring in extraction and bulk density data
pathbringin = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/"
extractiondata <- read.csv(paste(pathbringin, "Soil-Data-RawFolders/Field-Extraction-Log/ExtractionLogCompiled/extractionlogcsv.csv", sep = ""), stringsAsFactors=FALSE)
BDdata <- read.csv(paste(pathbringin, "Soil-Data-Rprocessed/soilbulkdensity_processed.csv", sep = ""), stringsAsFactors=FALSE)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rprocessed/"


########################################################################
# ADD IN BULK DENSITY COLUMN

BD <- BDdata[BDdata$SampleType=="combo",]
BD <- BD[c(1,13,14)]
extractiondata <- merge(extractiondata, BD, all=TRUE)


########################################################################
# CUT DOWN TO ONLY GOOD SAMPLES, DEAL WITH DATA QUALITY

# get rid of blanks
cols <- "Site"
extractionDF <- extractiondata[!rowSums(is.na(extractiondata[cols])), ]
# get rid of any samples I marked as "discard N"
extractionDF <- extractionDF[extractionDF$DiscardN != "Y", ]

# a bunch of the CENA nitr blank corrected data is negative; fix this
# (subtracted a mean blank value from a NO3 value set to the smallest blank value)
extractionDF$CENAnitrppmBnkCor <- ifelse(extractionDF$CENAnitrppmBnkCor<0,0,extractionDF$CENAnitrppmBnkCor)
extractionDF$CENAammppmBnkCor <- ifelse(extractionDF$CENAammppmBnkCor<0,0,extractionDF$CENAammppmBnkCor)



########################################################################
# FIND TOTAL EXTRACTANT VOLUME

# grams of soil water that was in your sample
extractionDF$SampleSoilWater_g <- with(extractionDF, ExtractionSoilWeight-((1-((SoilWetWeight_DarroMinusBaggie-SoilDryWeight_TownMinusPapBag)/SoilWetWeight_DarroMinusBaggie))*ExtractionSoilWeight))

# convert g to L
extractionDF$SampleSoilWater_L <- with(extractionDF, SampleSoilWater_g*0.001)

# add water from soil sample to the 50 or 25 mLs you added as an extractant
extractionDF$TotalExtractantVol_L <- with(extractionDF, SampleSoilWater_L+(ExtractantVol*0.001))



########################################################################
# CONVERT NO3, NH4 FROM PPM TO mg N g-1

# get from mg/L to mgN vol of extractant times ppm (note, ppm = mgL)
extractionDF$NO3_N_mgN <- with(extractionDF, TotalExtractantVol_L*CENAnitrppmBnkCor)
extractionDF$NH4_N_mgN <- with(extractionDF, TotalExtractantVol_L*CENAammppmBnkCor)
extractionDF$NO3_N_NH4_N_mgN <- with(extractionDF, NO3_N_mgN + NH4_N_mgN)

# get dry-equivalent grams of soil you extracted from
extractionDF$ExtractionSoilWeight_DryEquiv <- with(extractionDF, (SoilDryWeight_TownMinusPapBag/SoilWetWeight_DarroMinusBaggie)*ExtractionSoilWeight)

# convert from mgN to mgN/g of soil
extractionDF$NO3_N_mgNg <- with(extractionDF, NO3_N_mgN/ExtractionSoilWeight_DryEquiv)
extractionDF$NH4_N_mgNg <- with(extractionDF, NH4_N_mgN/ExtractionSoilWeight_DryEquiv)
extractionDF$NO3_N_NH4_N_mgNg <- with(extractionDF, NO3_N_NH4_N_mgN/ExtractionSoilWeight_DryEquiv)



########################################################################
# CALCULATE NET AMMONIFICATION, NITRIFICATION AND MINERALIZATION RATES

# length of incubation
extractionDF$DateLab1 <- mdy(extractionDF$DateLab1)
extractionDF$DateLab2Inc <- mdy(extractionDF$DateLab2Inc)
extractionDF$IncubationDays <- as.period(extractionDF$DateLab2Inc - extractionDF$DateLab1,units=c("days"))
extractionDF$IncubationDays <- day(extractionDF$IncubationDays)

# bulk density core depth
extractionDF$BDCoreDepth <- 10

# make new column with unique name to match
extractionDF$extlazytest <- "24"
extractionDF$extlazytest[grep("48", extractionDF$sampleID)] <- "48"
extractionDF$easycallname <- do.call(paste, c(extractionDF[c("Site", "Chamber", "DateField", "extlazytest")], sep = "_")) 

# start the appropriate columns in extractionDF
extractionDF$NO3_N_mgNg_FinalMinusInitial_perDay <- "NA"
extractionDF$NH4_N_mgNg_FinalMinusInitial_perDay <- "NA"
extractionDF$NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay <- "NA"
extractionDF$NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- "NA"
extractionDF$NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- "NA"
extractionDF$NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- "NA"

chamberstoprocess <- na.omit(unique(extractionDF$easycallname ))

for (i in 1:length(chamberstoprocess)) {
      
      # get the two vials that go with that unique chamber on that date
      vials <- subset(extractionDF, easycallname == chamberstoprocess[i])
      
      # make sure you have a before and after
      if(dim(vials)[1]==2) {
            
            # net nitrification rate
            # mg N g soil-1 day-1
            # NO3-N_mgNg_FinalMinusInitial_perDay
            # ( NO3-N_mgNg incub value - NO3-N_mgNg ext value ) / incub days
            NO3_N_mgNg_FinalMinusInitial_perDay <- (vials$NO3_N_mgN[vials$extinc=="inc"] - vials$NO3_N_mgN[vials$extinc=="ext"])/vials$IncubationDays[vials$extinc=="inc"]
            extractionDF$NO3_N_mgNg_FinalMinusInitial_perDay[extractionDF$easycallname == chamberstoprocess[i]] <- NO3_N_mgNg_FinalMinusInitial_perDay
            
            # net ammonification rate
            # mg N g soil-1 day-1
            # NH4-N_mgNg_FinalMinusInitial_perDay
            # ( NH4-N_mgNg incub value - NH4-N_mgNg ext value ) / incub days
            NH4_N_mgNg_FinalMinusInitial_perDay <- (vials$NH4_N_mgN[vials$extinc=="inc"] - vials$NH4_N_mgN[vials$extinc=="ext"])/vials$IncubationDays[vials$extinc=="inc"]
            extractionDF$NH4_N_mgNg_FinalMinusInitial_perDay[extractionDF$easycallname == chamberstoprocess[i]] <- NH4_N_mgNg_FinalMinusInitial_perDay
            
            # net mineralization rate
            # mg N g soil-1 day-1
            # NO3-N_NH4-N_mgNg_FinalMinusInitial_perDay
            # ( NO3-N_NH4-N_mgNg incub value - NO3-N_NH4-N_mgNg ext value ) / incub days
            NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay <- (vials$NO3_N_NH4_N_mgN[vials$extinc=="inc"] - vials$NO3_N_NH4_N_mgN[vials$extinc=="ext"])/vials$IncubationDays[vials$extinc=="inc"]
            extractionDF$NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay[extractionDF$easycallname == chamberstoprocess[i]] <- NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay
            
            # net nitrification rate, per area basis
            # mg N m-2 day-1
            # NO3-N_mgNg_FinalMinusInitial_perDay_AreaBasis
            # NO3_N_mgNg_FinalMinusInitial_perDay * BulkDensity * BDCoreDepth * 10000
            NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- NO3_N_mgNg_FinalMinusInitial_perDay * vials$BulkDensity_gcm3_use[vials$extinc=="ext"] * vials$BDCoreDepth[vials$extinc=="ext"] * 10000
            extractionDF$NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis[extractionDF$easycallname == chamberstoprocess[i]] <- NO3_N_mgNg_FinalMinusInitial_perDay_AreaBasis
            
            # net ammonification rate, per area basis
            # mg N m-2 day-1
            # NH4-N_mgNg_FinalMinusInitial_perDay_AreaBasis
            # NH4_N_mgNg_FinalMinusInitial_perDay * BulkDensity * BDCoreDepth * 10000
            NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- NH4_N_mgNg_FinalMinusInitial_perDay * vials$BulkDensity_gcm3_use[vials$extinc=="ext"] * vials$BDCoreDepth[vials$extinc=="ext"] * 10000
            extractionDF$NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis[extractionDF$easycallname == chamberstoprocess[i]] <- NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis
            
            # net mineralization rate, per area basis
            # mg N m-2 day-1
            # NO3-N_NH4-N_mgNg_FinalMinusInitial_perDay_AreaBasis
            # NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay * BulkDensity * BDCoreDepth * 10000
            NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis <- NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay * vials$BulkDensity_gcm3_use[vials$extinc=="ext"] * vials$BDCoreDepth[vials$extinc=="ext"] * 10000
            extractionDF$NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis[extractionDF$easycallname == chamberstoprocess[i]] <- NO3_N_NH4_N_mgNg_FinalMinusInitial_perDay_AreaBasis
            
      } 
      
}



########################################################################
# SAVE CSV WITH POOL SIZES AND RATES FOR EACH CHAMBER

# so can merge easily with tanguro master data sheet
extractionDF$easycallname_TMDS_Soil <- do.call(paste, c(extractionDF[c("Site", "Chamber", "DateField")], sep = "_")) 

# save csv file
write.csv(extractionDF, file=paste(pathsavefiles, "extractionDF_processed.csv", sep = ""), row.names=FALSE)  




########################################################################
# NOTES AND TESTING

# note that there are many NA values remaining in these 6 N transformation columns
# this is because if there isn't a reported value for CENAnitrppm AND CENAammppm for the incubation AND the extraction vial, that transformation is not calculated since there isn't a reliable before-after
# (if needed, could relax this stringent data quality decision and use a mean after value (or before value))






