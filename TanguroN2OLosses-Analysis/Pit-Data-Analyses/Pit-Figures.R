# Pit-Figures.R
# taking trace gas vial data associated with soil pits and making a tidy data set
# disrupted N project
# CS O'Connell, UMN EEB/IonE

# requires files created in GC-Rcode-fileloop.R

# output products:
# pitgasfull.csv : master csv of soil pit gas sampling


########################################################################
# BRING IN DATA / PREP

pitgasfull <- read.csv("~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Pit-Data-Rprocessed/pitgasfull.csv", stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(lubridate)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/GC-Data-Raw-R/Pit-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso011code_TanguroN2OLosses/TanguroN2OLosses-Analysis/Pit-Data-Analyses/PitGasFigures/"

# create a col to assign a color in the graphs
pitgasfull <- transform(pitgasfull, color.use = ifelse(LUtype=="M", as.character("darkorange"), ifelse(LUtype=="F", as.character("darkgreen"), as.character("darkblue"))))

# create a col to assign a better name to each land use
pitgasfull <- transform(pitgasfull, LUname = ifelse(LUtype=="M", as.character("Soya/Maize DC"), ifelse(LUtype=="F", as.character("Forest"), as.character("Soya SC"))))


########################################################################
# THROW OUT ONE WEIRD DATA POINT

# get rid of that one weird rerun vial that is way off from the other two
rerunid <- grep("F1-K4-250cm-C_rerun", pitgasfull$SampleName)
pitgasfull$ngN_cm3_N2O[rerunid] <- NA
pitgasfull$ngC_cm3_CO2[rerunid] <- NA
pitgasfull$ngC_cm3_CH4[rerunid] <- NA
pitgasfull$N2Oppm[rerunid] <- NA
pitgasfull$CO2ppm[rerunid] <- NA
pitgasfull$CH4ppm[rerunid] <- NA


########################################################################
# DESCRIPTIVE STATS SUMMARY

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE
summarytab1 <- summarySE(data=pitgasfull, measurevar="N2Oppm", c("pitID", "sampledepth", "SampleDate", "LUtype", "LUname", "color.use", "Month"), na.rm=TRUE, renameallcols=TRUE)
summarytab2 <- summarySE(data=pitgasfull, measurevar="CO2ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)
summarytab3 <- summarySE(data=pitgasfull, measurevar="CH4ppm", c("pitID", "sampledepth", "SampleDate"), na.rm=TRUE, renameallcols=TRUE)

# join
pitgassummary <- join(x = summarytab1, y = summarytab2, by = c("pitID", "sampledepth", "SampleDate", "N"))
pitgassummary <- join(x = pitgassummary, y = summarytab3, by = c("pitID", "sampledepth", "SampleDate", "N"))




########################################################################
# SIMPLE SCATTERPLOT

p1 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanN2Oppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("sample depth (cm)") + geom_errorbar(aes(ymin=meanN2Oppm-sdN2Oppm, ymax=meanN2Oppm+sdN2Oppm), width=5)

p2 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCO2ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("sample depth (cm)") + geom_errorbar(aes(ymin=meanCO2ppm-sdCO2ppm, ymax=meanCO2ppm+sdCO2ppm), width=5)

p3 <- ggplot(pitgassummary, aes(x=sampledepth, y=meanCH4ppm)) + geom_point(shape=1) + geom_line(aes(color=Month)) + coord_flip() + scale_x_reverse() + facet_grid(pitID ~ .) + xlab("sample depth (cm)") + geom_errorbar(aes(ymin=meanCH4ppm-sdCH4ppm, ymax=meanCH4ppm+sdCH4ppm), width=5)

# individ gas graphs
png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-N2O.png", sep=""),width=6,height=6,units="in",res=150)
p1 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CO2.png", sep=""),width=6,height=6,units="in",res=150)
p2 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations-CH4.png", sep=""),width=6,height=6,units="in",res=150)
p3 + labs(title = "Trace Gas Concentration, Tanguro Soil Pits") 
dev.off()

# grid.arrange
png(file = paste(pathsavefigs, "soilpit-tracegasconcentrations.png", sep=""),width=12,height=6,units="in",res=150)
grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, main="Trace Gas Concentration, Tanguro Soil Pits")
dev.off()



########################################################################
# SAVE CSV

# save summary as csv
write.csv(pitgassummary, file=paste(pathsavefiles, "pitgassummary.csv", sep = ""), row.names=FALSE)  





########################################################################
# NOTES AND TESTING




