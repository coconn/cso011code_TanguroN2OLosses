# Soil-Data-Rcode.R
# taking soil data from Tanguro trace gas project and making tidy data sets
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# output products:
# soilNfull.csv : master csv of nitrogen extraction data


########################################################################
# SET UP

library(data.table)
library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(lubridate)
#library(XLConnect)
library(xlsx)
library(reshape)
library(tidyr)
library(stringi)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Soil-Data-Figures/"

# how many excel files are there
soildatafolder = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/Soil N Extractions/"
filenames <- list.files(soildatafolder, pattern="*.xls*")



########################################################################
# WET SEASON 2014: BRING IN DATA

# filesnames[1], filesnames[5], and filesnames[4] go together

# filenames[1] = "AMMONIUN CHRISTINE.xls"
# ammonium concentrations, no column with CSO label info
# many sheets, where each sheet is a samples run on a different date
# collated into a single sheet by hand, 3-31-2015
runsheet <- read.xlsx(paste(soildatafolder,filenames[1],sep=""),"CSOcombinedbyhand")
runsheet <- as.data.frame(runsheet, stringsAsFactors=FALSE)
# fix weird excel date thing
runsheet$Date <- as.character(runsheet$Date)
runsheet$Date[runsheet$Date=="42039"] <- "04-02-2015"; runsheet$Date[runsheet$Date=="42037"] <- "02-02-2015"
runsheetAmm2014 <- runsheet

# filenames[5] = "NITRATE CHRISTINE.xls"
# nitrate concentrations, no column with CSO label info
# many sheets, where each sheet is a samples run on a different date
# collated into a single sheet by hand, 3-31-2015
runsheet <- read.xlsx2(paste(soildatafolder,filenames[5],sep=""),"CSOcombinedbyhand")
runsheet <- as.data.frame(runsheet, stringsAsFactors=FALSE)
# fix weird excel date thing
runsheet$Date <- as.character(runsheet$Date)
runsheet$Date[runsheet$Date=="42126"] <- "05-02-2015"; runsheet$Date[runsheet$Date=="42157"] <- "06-02-2015"; runsheet$Date[runsheet$Date=="42279"] <- "10-02-2015"; runsheet$Date[runsheet$Date=="42340"] <- "12-02-2015"
runsheetNitr2014 <- runsheet

# filenames[4] = "N MINERAL CHRISTINE 24-03-14.xls"
# two sheets, both are kind of a mess
runsheet <- read.xlsx(paste(soildatafolder,filenames[4],sep=""),2, colClasses="character")

# do these three files have the same data within?
tmp1 <- as.character(runsheet[8:19,6])
tmp2 <- as.character(runsheetAmm2014[4:15,2])
identical(tmp1,tmp2) #ammonium checks out
tmp1 <- as.character(runsheet[14:23,5])
tmp2 <- as.character(round(as.numeric(as.character(runsheetNitr2014[9:18,2])),digits=5))
identical(tmp1,tmp2) #nitrate checks out

# so, keep filenames[4]
df <- runsheet; rm(runsheetAmm2014, runsheetNitr2014,runsheet)

# make df the right dimensions
df <- df[6:573,1:7]
colnames(df) = c("num","sampleID","capnotes","labelnotes","listdiffs","nitrppm","ammppm")
head(df)


########################################################################
# WET SEASON 2014: CLEAN UP DATA

# make sure that no sites are still labeled SD or SM
df$sampleID <- gsub("SM","S3", df$sampleID, fixed=TRUE)
df$sampleID <- gsub("SD","S2", df$sampleID, fixed=TRUE)

# put info from sampleID into several columns

# extraction or incubation
df$extinc <- -9999
df$extinc[grep("ext", df$sampleID)] <- "ext"; df$extinc[grep("inc", df$sampleID)] <- "inc"; df$extinc[grep("bnk", df$sampleID)] <- "bnk"

# date
df$sampleID <- as.character(df$sampleID)
df$datestr <- stri_sub(df$sampleID,-8,-1)
df$date <- dmy(df$datestr) 

# site
df$Site <- -9999
df$Site[grep("F1", df$sampleID)] <- "F1"; df$Site[grep("F2", df$sampleID)] <- "F2"; df$Site[grep("F3", df$sampleID)] <- "F3"
df$Site[grep("M1", df$sampleID)] <- "M1"; df$Site[grep("M2", df$sampleID)] <- "M2"; df$Site[grep("M3", df$sampleID)] <- "M3"
df$Site[grep("S1", df$sampleID)] <- "S1"; df$Site[grep("S2", df$sampleID)] <- "S2"; df$Site[grep("S3", df$sampleID)] <- "S3"
df$Site[grep("bnk", df$sampleID)] <- "NA"

# land use
df$LUtype <- -9999
df$LUtype[grep("M", df$sampleID)] <- "M"; df$LUtype[grep("F", df$sampleID)] <- "F"; df$LUtype[grep("S", df$sampleID)] <- "S"
df$LUtype[grep("bnk", df$sampleID)] <- "NA"

# chamber
df$Chamber <- -9999
df$Chamber[grep("A_", df$sampleID)] <- "A"; df$Chamber[grep("B_", df$sampleID)] <- "B"; df$Chamber[grep("C_", df$sampleID)] <- "C"; df$Chamber[grep("D_", df$sampleID)] <- "D"; df$Chamber[grep("E_", df$sampleID)] <- "E"
df$Chamber[grep("bnk", df$sampleID)] <- "NA"

# include the 24 hr vs. 48 test as a column


# some rows are repeats of the row above, so take the mean of those two rows
# listdiffs, "same decription"



########################################################################
# DRY SEASON 2013: BRING IN DATA

#DEAL WITH LATER

# # filenames[3] = "Christine O Connell NITRATO corrigido.xls"
# # one sheet with "corrected" nitrate and the other with the raw data (including standards)
# runsheet <- read.xlsx(paste(soildatafolder,filenames[3],sep=""),1)
# 
# # filenames[2] = "AMOSTRAS CHRISTINE 2013 ENCONTRADAS.xlsx"
# # ammonium and nitrate concentrations, includes CSO label info
# # only one sheet, given nicer column names by hand, 3-31-2015
# # no included lab dates
# runsheet <- read.xlsx(paste(soildatafolder,filenames[2],sep=""),"CSOcombinedbyhand")
# runsheet <- as.data.frame(runsheet, stringsAsFactors=FALSE)
# runsheet$DESCRIÇÃO <- as.character(runsheet$DESCRIÇÃO)
# runsheet2 <- runsheet










# initialize data.table



vialDFfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/GC-Data-Rprocessed/vialDFfull.csv", stringsAsFactors=FALSE)

# make new column to ensure no repeats (easycallname = unique per chamber)
vialDFfull$easycallname <- do.call(paste, c(vialDFfull[c("Site", "SampleDate", "Chamber")], sep = "_")) 

#in case I want to see all the names outside of R environment
#write.csv(vialDFfull, file=paste("vialDFfull_prac.csv", sep = ""), row.names=FALSE)  




########################################################################
# GET PIT DATA

# Pits: m8, k4, c2 (forest); mu (mutun, soy)

toMatch <- c("cm") #"M8", "K4", "MU", "C2" # C2 doesn't work because it goes with some side-by-side chamber trials
pitgas <- subset(vialDFfull, grepl(paste(toMatch,collapse="|"), vialDFfull$SampleName))



########################################################################
# ELIMINATE UNACCEPTABLE DATA

# no pressure vials
nopressureid <- pitgas$Pressure=="N"
NoPressureCount <- length(which(pitgas$Pressure=="N"))

pitgas$ngN_cm3_N2O[nopressureid] <- NA
pitgas$ngC_cm3_CO2[nopressureid] <- NA
pitgas$ngC_cm3_CH4[nopressureid] <- NA

pitgas$N2Oppm[nopressureid] <- NA
pitgas$CO2ppm[nopressureid] <- NA
pitgas$CH4ppm[nopressureid] <- NA

# print info
print(paste("VIAL PRESSURE INFO: there was/were ", NoPressureCount, " vial(s) with no pressure.", sep = ""))

# some vials got repeated because of GC autosampler problems
rerunid <- grep("Rerun_", pitgas$SampleName)

pitgas$ngN_cm3_N2O[rerunid] <- NA
pitgas$ngC_cm3_CO2[rerunid] <- NA
pitgas$ngC_cm3_CH4[rerunid] <- NA

pitgas$N2Oppm[rerunid] <- NA
pitgas$CO2ppm[rerunid] <- NA
pitgas$CH4ppm[rerunid] <- NA

# any other vials that didn't seem to get sampled?
nodata <- pitgas$N2Oraw<1.0

pitgas$ngN_cm3_N2O[nodata] <- NA
pitgas$ngC_cm3_CO2[nodata] <- NA
pitgas$ngC_cm3_CH4[nodata] <- NA

pitgas$N2Oppm[nodata] <- NA
pitgas$CO2ppm[nodata] <- NA
pitgas$CH4ppm[nodata] <- NA

# recall that all three of these vials got rerun in the GC, so there is no data that is truly missing, only blank rows to be struck

# some of the land use codes are "r" because of the rerun vials
# site and LUtype have "re" and "r"
Rid <- grep("R", pitgas$Site)

samplenamesRe <- pitgas$SampleName[Rid]
sitetmp <- substr(samplenamesRe, 7, 8)
LUtmp <- substr(samplenamesRe, 7, 7)

pitgas$Site[Rid] <- sitetmp
pitgas$LUtype[Rid] <- LUtmp


########################################################################
# ADD USEFUL COLUMNS

# pit ID
pitgas$pitID <- -9999
pitgas$pitID[grep("M8", pitgas$SampleName)] <- "M8"
pitgas$pitID[grep("K4", pitgas$SampleName)] <- "K4"
pitgas$pitID[grep("MU", pitgas$SampleName)] <- "MU"
pitgas$pitID[grep("C2", pitgas$SampleName)] <- "C2"

# depth
pitgas$sampledepth <- -9999
pitgas$sampledepth[grep("15cm", pitgas$SampleName)] <- 15
pitgas$sampledepth[grep("40cm", pitgas$SampleName)] <- 40
pitgas$sampledepth[grep("75cm", pitgas$SampleName)] <- 75
pitgas$sampledepth[grep("150cm", pitgas$SampleName)] <- 150
pitgas$sampledepth[grep("250cm", pitgas$SampleName)] <- 250
pitgas$sampledepth[grep("350cm", pitgas$SampleName)] <- 350
pitgas$sampledepth[grep("450cm", pitgas$SampleName)] <- 450


########################################################################
# SAVE CSV

# save soilNfull as csv file
soilNfull <- soilN
write.csv(soilNfull, file=paste(pathsavefiles, "soilNfull.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING




