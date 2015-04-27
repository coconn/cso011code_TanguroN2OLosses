# BulkDensityCalcs-Rcode.R
# taking vial data and converting it into fluxes
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files combined by hand in Soil-Data-Raw-R/Soil-Data-RawFolders/Bulk Density
# takes soil dry weight data from Tanguro and calculates bulk density for each of Christine's N2O sites

# output products:
# soilbulkdensity_processed.csv : bulk density summary CSV
# BDmethodcomparison.png: compares the results from the truth bar method (july 2013) with the bulk density ring method (nov 2014)



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
library(Hmisc)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-Rprocessed/"



########################################################################
# TANGURO BULK DENSITY COMBO FILE: BRING IN DATA

df <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/Soil-Data-Raw-R/Soil-Data-RawFolders/Bulk Density/NCD-bulkdensity-combined-data.csv", stringsAsFactors=FALSE)


########################################################################
# SUMMARY INFO

source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

BDsummary <- summarySE(data=df, measurevar="BulkDensity_gcm3", groupvars=c("Site","SampleMonth","SampleType"), na.rm=TRUE)

# weighted mean info to combine the two BD measurement methods
tmp <- ddply(BDsummary, c("Site"), summarise, 
      wmn = wtd.mean(BulkDensity_gcm3, N),
      wv = wtd.var(BulkDensity_gcm3, N),
      wsd = sqrt(wv),
      mn = mean(BulkDensity_gcm3))
tmp$SampleMonth <- "combo"
tmp$SampleType <- "combo"

# put into one df
BDsummary2 <- rbind.fill(BDsummary,tmp) # allows the dfs to have different columns (fills in needed NAs)
# columns that IDs which BD measurement and uncertainty to use
BDsummary2 <- transform(BDsummary2, BulkDensity_gcm3_use = ifelse(SampleMonth=="combo", wmn, BulkDensity_gcm3))
BDsummary2 <- transform(BDsummary2, BulkDensity_gcm3_uncertainty_use = ifelse(SampleMonth=="combo", wsd, sd))


########################################################################
# HOW WELL DO THE METHODS COMPARE?

# reorder the bars so they are chronological and then weighted mean
BDsummary2$SampleType2 <- factor(BDsummary2$SampleType, levels=c("Truth bar hole", "BD ring", "combo")) # reprints as factors with levels

png(file = paste(pathsavefiles, "BDmethodcomparison.png", sep=""),width=8,height=6,units="in",res=400)
# barplot
ggplot(data=BDsummary2, aes(x=Site, y=BulkDensity_gcm3_use, group=SampleType2,fill=SampleType2)) + geom_bar(position='dodge', stat='identity') + geom_errorbar(aes(ymax=BulkDensity_gcm3_use+BulkDensity_gcm3_uncertainty_use, ymin=BulkDensity_gcm3_use-BulkDensity_gcm3_uncertainty_use), position=position_dodge(0.9),width=.25, data=BDsummary2) + ylab("Bulk Density, g/cm^3") + scale_fill_discrete(name="Sample Method\nAnd Month", breaks=c("Truth bar hole", "BD ring", "combo"), labels=c("Truth Bar Method, \nJuly 2013", "BD Ring Method, \nNov. 2014", "Weighted Mean of \nRing/Bar Methods")) + theme(legend.position="bottom")
dev.off()


########################################################################
# SAVE CSV BULK DENSITY SUMMARY TO USE

# save df as csv file
write.csv(BDsummary2, file=paste(pathsavefiles, "soilbulkdensity_processed.csv", sep = ""), row.names=FALSE)  




########################################################################
# NOTES AND TESTING




