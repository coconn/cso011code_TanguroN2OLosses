# ExtractionCalcs-Rcode.R
# taking vial data and converting it into fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in XXXXX place
# takes soil extraction data from CENA/MBL and calculates [NO3], [NH4], [NO3+NH4], net N min, net N nitr, net immobilization (per gram basis and per area basis)

# output products:
# soilextractiondata_processed.csv: reports NO3 and NH4 as mg N/g soil and not ppm
# chambersoilNmaster.csv



########################################################################
# BRING IN DATA, ETC.

extractiondata <- read.csv("~/Downloads/ncd-calc-vals-practiceR.csv", stringsAsFactors=FALSE)

library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rprocessed/"


########################################################################
# CORRECT PPMS USING THE BLANKS

##### DO THIS LATER

# for now
extractiondata$NO3_N_mgL_blankcorr <- extractiondata$NO3_N_mgL
extractiondata$NH4_N_mgL_blankcorr <- extractiondata$NH4_N_mgL


########################################################################
# FIND TOTAL EXTRACTANT VOLUME

# grams of soil water that was in your sample
extractiondata$SampleSoilWater_g <- with(extractiondata, ExtractionSoilWeight-((1-((SoilWetWeight_DarroMinusBaggie-SoilDryWeight_TownMinusPapBag)/SoilWetWeight_DarroMinusBaggie))*ExtractionSoilWeight))

# convert g to L
extractiondata$SampleSoilWater_L <- with(extractiondata, SampleSoilWater_g*0.001)

# add water from soil sample to the 50 or 25 mLs you added as an extractant
extractiondata$TotalExtractantVol_L <- with(extractiondata, SampleSoilWater_L+(ExtractantVol*0.001))


########################################################################
# CONVERT NO3, NH4 FROM PPM TO mg N g-1

# get from mg/L to mgN vol of extractant times ppm (note, ppm = mgL)
extractiondata$NO3_N_mgN <- with(extractiondata, TotalExtractantVol_L*NO3_N_mgL)
extractiondata$NH4_N_mgN <- with(extractiondata, TotalExtractantVol_L*NH4_N_mgL)
extractiondata$NO3_N_NH4_N_mgN <- with(extractiondata, NO3_N_mgN + NH4_N_mgN)

# get dry-equivalent grams of soil you extracted from
extractiondata$ExtractionSoilWeight_DryEquiv <- with(extractiondata, (SoilDryWeight_TownMinusPapBag/SoilWetWeight_DarroMinusBaggie)*ExtractionSoilWeight)

# convert from mgN to mgN/g of soil
extractiondata$NO3_N_mgNg <- with(extractiondata, NO3_N_mgN/ExtractionSoilWeight_DryEquiv)
extractiondata$NH4_N_mgNg <- with(extractiondata, NH4_N_mgN/ExtractionSoilWeight_DryEquiv)
extractiondata$NO3_N_NH4_N_mgNg <- with(extractiondata, NO3_N_NH4_N_mgN/ExtractionSoilWeight_DryEquiv)



########################################################################
# SAVE CSV WITH CONVERTED NO3 and NH4 CONCENTRATION VALS

# note that every step above was completed for the initial "ext"/"extraction" and final "inc"/"incubation" sample

# save csv file
write.csv(extractiondata, file=paste(pathsavefiles, "soilextractiondata_processed.csv", sep = ""), row.names=FALSE)  



########################################################################
# SET UP chambersoilN DATA FRAME

# start off chamber soil N property table with the initial extraction data 

# remove places where test type is "inc" or "doc" or site is a blank
pattern <- "Inc|DOC"; tmp <- grep(pattern, extractiondata$TestType)
pattern <- "Blank|blank|blk|bnk|Blk|Bnk"; tmp2 <- grep(pattern, extractiondata$Site)
tmp3 <- union(tmp,tmp2)

# build new table
keepcols <- c("Site","Chamber","DateField","DateLabBegin","SampleID","NO3_N_mgNg","NH4_N_mgNg","NO3_N_NH4_N_mgNg")
chambersoilN <- extractiondata[-tmp3,keepcols]

# fill in DateIncubLabBegin, IncubationDays


# fill in bulk density values per site

# bring in BD results table
##### FAKE FOR NOW
BD <- round(runif(9,1.2,1.6), 2)







# create a col to assign a color in the graphs
pitgasfull <- transform(pitgasfull, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))


# chamber
df$Chamber <- -9999
df$Chamber[grep("A_", df$sampleID)] <- "A"; df$Chamber[grep("B_", df$sampleID)] <- "B"; df$Chamber[grep("C_", df$sampleID)] <- "C"; df$Chamber[grep("D_", df$sampleID)] <- "D"; df$Chamber[grep("E_", df$sampleID)] <- "E"
df$Chamber[grep("bnk", df$sampleID)] <- "NA"




########################################################################
# CALCULATE NET AMMONIFICATION, NITRIFICATION AND MINERALIZATION RATES



########################################################################
# SAVE POOL SIZES AND RATES FOR EACH CHAMBER INTO CLEAN CSV




write.csv(chambersoilN, file=paste(pathsavefiles, "chambersoilNmaster.csv", sep = ""), row.names=FALSE)  



########################################################################
# NOTES AND TESTING

# blank correction step is a placeholder



