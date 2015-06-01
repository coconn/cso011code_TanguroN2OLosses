# Process-soilNlabresults-Cleaning-Rcode.R.R
# taking soil data from Tanguro trace gas project that I received directly from CENA and MBL
# parsing the data so it's manageable and has appropriate columns
# right now this is only wet season data
#
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# output products:
# soilNlabresults.csv : master csv of nitrogen extraction data from the CENA and MBL labs
# this is just a data cleaning file
#
# soilNlabresults.csv then got sorted by hand into "Field-Extraction-Log/ExtractionLogCompiled/extractionlogcsv.csv"
#


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
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/Field-Extraction-Log/ExtractionLogCompiled/"

# how many excel files are there
soildatafolder = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/Soil N Extractions/CENA data - everything received/"
filenames <- list.files(soildatafolder, pattern="*.xls*")



########################################################################
# WET SEASON 2014, CENA RESULTS: BRING IN DATA

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
# WET SEASON 2014, CENA RESULTS: CLEAN UP DATA

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

# other useful info columns

# include the 24 hr vs. 48 test as a column
df$test2448 <- "NA"
df$test2448[grep("24", df$labelnotes)] <- "24"; df$test2448[grep("48", df$labelnotes)] <- "48";

# include whether the vial was acid washed as a column
df$acidwashed <- "NA"
df$acidwashed[grep("(aw)", df$listdiffs)] <- "acidwashedvial"
# does that mean that every vial after that date was also acid washed?  go back and check on this; mark it in the column if so.

# that this data is from CENA
df$lab <- "CENA"


########################################################################
# SAVE CSV WET SEASON CENA RESULTS

# save df for wet season as csv file
# write.csv(df, file=paste(pathsavefiles, "Soil-Data-RawFolders/Soil N Extractions/PreHandSorting-files/N-MINERAL-CHRISTINE-24-03-14-Use-by-hand.csv", sep = ""), row.names=FALSE)  

# after looking at the data in excel, some problems still to be dealt with:
# some rows are repeats of the row above them, so take the mean of those two rows
# a bunch of the data is no good; delete those rows
# handle this in the next section of code


########################################################################
# FIX WET SEASON CENA DATA PROBLEMS THAT SHOULDN'T BE FIXES BY HAND

# keep things as.character so you can use grep
df$nitrppm <- as.character(df$nitrppm)
df$ammppm <- as.character(df$ammppm)

# "two samples, same description", then average that row with the row below it
tmp <- grep("samples, same description", df$listdiffs) # avoid also picking df$listdiffs[348:352]
for(i in 1:length(tmp)) {
      # get info
      ind <- tmp[i]
      meannitr <- mean(as.numeric(df$nitrppm[ind:(ind+1)]), na.rm=TRUE)
      meanamm <- mean(as.numeric(df$ammppm[ind:(ind+1)]), na.rm=TRUE)
      # make a new row with the avgs
      rowfill <- df[ind,]
      rowfill$listdiffs <- "mean of two rows"
      rowfill$nitrppm <- as.character(meannitr)
      rowfill$ammppm <- as.character(meanamm)
      # put row into df
      df <- rbind(df,rowfill)
}
# get rid of rows that we just averaged
gone <- c(tmp,tmp+1)
df <- df[-gone,]

# "insufficient sample" to NA concentration
df$nitrppm[grep("Insufficient", df$nitrppm)] <- "NA"
df$ammppm[grep("Insufficient", df$ammppm)] <- "NA"

# remove "sample absent" rows
df$nitrppm[grep("absent", df$nitrppm)] <- "NA"
df$ammppm[grep("absent", df$ammppm)] <- "NA"

# places where neither nitr or amm have a value, remove the row
tmp <- grep("NA", df$nitrppm)
tmp2 <- grep("NA", df$ammppm)
tmp3 <- intersect(tmp,tmp2)
df <- df[-tmp3,]


########################################################################
# WET SEASON 2014, MBL RESULTS: BRING IN DATA

soildatafolder2 = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/Soil N Extractions/MBL Data/"
dfmbl <- read.xlsx(paste(soildatafolder2,"OConnellSamples.xlsx",sep=""),"Sheet1")
# take out final row
dfmbl <- dfmbl[1:50,]

# get the columns right for this

# extraction or incubation
dfmbl$extinc <- -9999
dfmbl$extinc[grep("EXT", dfmbl$Inc.or.Ext.)] <- "ext"; dfmbl$extinc[grep("INC", dfmbl$Inc.or.Ext.)] <- "inc"; dfmbl$extinc[grep("Blank|Bnk", dfmbl$Site.chamber)] <- "bnk"

# make sure that no sites are still labeled SD or SM
dfmbl$Site.chamber <- gsub("SM","S3", dfmbl$Site.chamber, fixed=TRUE)
dfmbl$Site.chamber <- gsub("SD","S2", dfmbl$Site.chamber, fixed=TRUE)

# site
dfmbl$Site <- -9999
dfmbl$Site[grep("F1", dfmbl$Site.chamber)] <- "F1"; dfmbl$Site[grep("F2", dfmbl$Site.chamber)] <- "F2"; dfmbl$Site[grep("F3", dfmbl$Site.chamber)] <- "F3"
dfmbl$Site[grep("M1", dfmbl$Site.chamber)] <- "M1"; dfmbl$Site[grep("M2", dfmbl$Site.chamber)] <- "M2"; dfmbl$Site[grep("M3", dfmbl$Site.chamber)] <- "M3"
dfmbl$Site[grep("S1", dfmbl$Site.chamber)] <- "S1"; dfmbl$Site[grep("S2", dfmbl$Site.chamber)] <- "S2"; dfmbl$Site[grep("S3", dfmbl$Site.chamber)] <- "S3"
dfmbl$Site[grep("Blank|Bnk", dfmbl$Site.chamber)] <- "NA"

# land use
dfmbl$LUtype <- -9999
dfmbl$LUtype[grep("M", dfmbl$Site.chamber)] <- "M"; dfmbl$LUtype[grep("F", dfmbl$Site.chamber)] <- "F"; dfmbl$LUtype[grep("S", dfmbl$Site.chamber)] <- "S"
dfmbl$LUtype[grep("Blank|Bnk", dfmbl$Site.chamber)] <- "NA"

# chamber
dfmbl$Chamber <- -9999
dfmbl$Chamber[grep("A", dfmbl$Site.chamber)] <- "A"; dfmbl$Chamber[grep("B", dfmbl$Site.chamber)] <- "B"; dfmbl$Chamber[grep("C", dfmbl$Site.chamber)] <- "C"; dfmbl$Chamber[grep("D", dfmbl$Site.chamber)] <- "D"; dfmbl$Chamber[grep("E", dfmbl$Site.chamber)] <- "E"
dfmbl$Chamber[grep("Blank|Bnk", dfmbl$Site.chamber)] <- "NA"

# date
dfmbl$date <- dfmbl$DateA

# include the 24 hr vs. 48 test as a column
dfmbl$test2448 <- "NA"
dfmbl$test2448[grep("24", dfmbl$aw24)] <- "24"; dfmbl$test2448[grep("48", dfmbl$aw24)] <- "48";

# include whether the vial was acid washed as a column
dfmbl$acidwashed <- "NA"
dfmbl$acidwashed[grep("AW", dfmbl$aw24)] <- "acidwashedvial"
# does that mean that every vial after that date was also acid washed?  go back and check on this; mark it in the column if so.

# that this data is from MBL
dfmbl$lab <- "MBL"

# smaller df
keep1 <- c("NO3.mg.L.","NH4.mg.L.","extinc","date","Site","LUtype","Chamber","test2448","acidwashed","lab")
dfmbl2 <- subset(dfmbl, select = keep1)


########################################################################
# MERGE MBL AND CENA DATA

totaldf <- rbind.fill(df, dfmbl2) # rfom plyr

########################################################################
# SAVE CSV TO USE

#colnames(df)
keep <- c("sampleID","nitrppm","ammppm","extinc","date","Site","LUtype","Chamber","test2448","acidwashed","lab", "NO3.mg.L.", "NH4.mg.L.")
df2 <- subset(totaldf, select = keep )
# switch to data.table
dt <- data.table(df2)

# save dt for wet season as csv file
write.csv(dt, file=paste(pathsavefiles, "Soil-Data-Rprocessed/soilNlabresults.csv", sep = ""), row.names=FALSE)  




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











########################################################################
# NOTES AND TESTING




